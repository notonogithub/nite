;;;;    router.lisp
;;;;
;;;;    This file is part of Nite
;;;;    Author: Pavel Penev (Lispegistus) <lispegistus@strangestack.com>
;;;;    Released under the Gnu Public License version 3
;;;;
;;;;    Nite is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    Nite is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with Nite. If not, see <http://www.gnu.org/licenses/>.

;; router

(defpackage #:nite.router
  (:use #:cl #:iterate)
  (:import-from #:bind
                #:bind)
  (:import-from #:alexandria
                #:hash-table-keys
                #:make-keyword)
  (:export
   #:parse-string-template
   #:unparse-string-template
   #:concatenate-string-template
   #:node
   #:node-route
   #:node-children
   #:node-path-component
   #:merge-nodes
   #:build-path
   #:add-route-at-path
   #:merge-node-at-path
   #:walk-nodes
   #:param-capture-children-p
   #:param-capture
   #:find-child
   #:find-path
   ;; high-level API
   #:*router*
   #:router
   #:router-route-map
   #:router-route-name-map
   #:rebuild-route-map
   #:find-route
   #:find-route-uri
   #:connect
   #:mount
   #:route-map-pretty))

(in-package #:nite.router)

;; Low-level implementation of the router tree

(defun parse-string-template (string-template)
  "Parses a string URI template to a list of path components. Input can include parameter capture variables
and a special * wildcard variable. The * must be at the end of the path, anything after it will be ignored.
Example:
  \"/foo/bar\" -> (\"\" \"foo\" \"bar\")
  \"/foo/:name|string/:id|integer\" -> (\"\" \"foo\" (:name :string) (:id :integer))
  \"/foo/:name/:id|integer\" -> (\"\" \"foo\" (:name :string) (:id :integer))
  \"/foo/*\" -> (\"\" \"foo\" (:* :wild)"
  (iter (for path-component in (str:split "/" string-template))
    (when (string= "*" path-component)
      (collect (cons :* :wild))
      (finish))
    (if (str:starts-with-p ":" path-component)
        (bind (((name &optional (parser-name "string")) (str:split "|" path-component)))
          (collect (cons (make-keyword (str:upcase (string-left-trim ":" name)))
                         (make-keyword (str:upcase parser-name)))))
        (collect path-component))))

(defun unparse-string-template (list-template &optional params)
  "Generate a string template from a parsed list template"
  (str:join "/" (iter (for path-component in list-template)
                      (collect (if (stringp path-component)
                                   path-component
                                   (if (eql :* (car path-component))
                                       "*"
                                       (format nil ":~A|~A" (car path-component) (cdr path-component))))))))

(defun concatenate-string-template (prefix template)
  "Concatenate two template strings while keeping slashes consistent"
  (let* ((trailing-slash (str:ends-with-p "/" template))
         (prefix (string-trim "/" prefix))
         (template (string-trim "/" template)))
    (cond ((and (str:emptyp prefix)
                (str:emptyp template))
           "/")
          ((str:emptyp prefix)
           (str:concat "/" template (if trailing-slash "/" "")))
          ((str:emptyp template)
           (str:concat "/" prefix (if trailing-slash "/" "")))
          (t (str:concat "/" prefix "/" template (if trailing-slash "/" ""))))))

;; tree code

(defclass node ()
  ((route :accessor node-route
          :initarg :route
          :initform nil
          :type (or null cons)
          :documentation "Route object, either nil or (cons <route-handler> <route-name>)") 
   (children :accessor node-children
             :initform (make-hash-table :test #'equalp)
             :type hash-table
             :documentation "Table of child nodes.")
   (path-component :accessor node-path-component
                   :initarg :path-component
                   :initform nil
                   :type (or null cons string)
                   :documentation "The path-component the node represents in the tree.")))

(defun merge-nodes (node1 node2 &optional path-component)
  "Create a new node and recursively node1 and node2 into it with node2 taking priority.
Optionally you can set the path-component of the new node manually, otherwise the path component
of node2 will be used."
  (bind (((:labels copy-node (node))
          (let ((new-node (make-instance 'node
                                         :path-component (node-path-component node)
                                         :route (node-route node))))
            (iter (for (key child) in-hashtable (node-children node))
                  (setf (gethash key (node-children new-node))
                        (copy-node child)))
            new-node))
         (new-node (make-instance 'node
                                  :route (or (node-route node2) (node-route node1))
                                  :path-component (or path-component (node-path-component node2))))
         (node1 (copy-node node1))
         (node2 (copy-node node2))
         (common-children (intersection (hash-table-keys (node-children node1))
                                        (hash-table-keys (node-children node2))))
         (node1-unique-children (set-difference (hash-table-keys (node-children node1))
                                                (hash-table-keys (node-children node2))))
         (node2-unique-children (set-difference (hash-table-keys (node-children node2))
                                                (hash-table-keys (node-children node1)))))
        (iter (for key in node1-unique-children)
              (setf (gethash key (node-children new-node))
                    (gethash key (node-children node1))))
        (iter (for key in node2-unique-children)
              (setf (gethash key (node-children new-node))
                    (gethash key (node-children node2))))
        (iter (for key in common-children)
              (setf (gethash key (node-children new-node))
                    (merge-nodes (gethash key (node-children node1))
                                 (gethash key (node-children node2)))))
        new-node))

(defun build-path (node path-component rest fn)
  "Build a path of nodes in the tree NODE, creating new nodes if they don't exist, call a function on the last child.
PATH-COMPONENT is the first path component, REST contains the rest of the path.
FN is a function of 3 arguments, the parent node, the last child node and the path component of the child."
  ;; Find the child or create it if it doesn't exist
  (let ((child
          (let ((key (if (consp path-component) (cdr path-component) path-component)))
            (or (gethash key (node-children node))
                (setf (gethash key (node-children node))
                      (make-instance 'node :path-component path-component))))))
    (if rest
        (build-path child (car rest) (cdr rest) fn)
        (funcall fn node child path-component))))

(defun add-route-at-path (root path route-handler &optional route-name)
  "Add a route to the root node at path."
  (if (or (string= path "/")
          (string= path "")
          (null path))
      (setf (node-route root) (cons route-handler route-name))
      (bind (((path-component . rest) (parse-string-template (string-left-trim "/" path))))
        (build-path root
                    path-component
                    rest
                    (lambda (parent-node child path-component)
                      (declare (ignore parent-node path-component))
                      (setf (node-route child) (cons route-handler route-name)))))))

(defun merge-node-at-path (root path node)
  "Find or create a new node to path, and then merge NODE with it."
  (if (or (string= path "/")
          (string= path "")
          (null path))
      (progn (setf (node-route root) (or (node-route node) (node-route root)))
             (merge-nodes root node))
      (bind (((path-component . rest) (parse-string-template (string-left-trim "/" path))))
        (build-path root
                    path-component
                    rest
                    (lambda (parent-node child path-component)
                      (setf (gethash (if (consp path-component)
                                         (cdr path-component)
                                         path-component)
                                     (node-children parent-node))
                            (merge-nodes child node path-component)))))))

(defun walk-nodes (function node &optional (path '("")))
  "Visit every node in a tree, applying FUNCTION to it.
FUNCTION takes 2 arguments, the child and the string path to the child."
  (iter (for (path-component child) in-hashtable (node-children node))
    (let* ((child-path (concatenate 'list path (if (stringp path-component)
                                                   (list path-component)
                                                   (list (node-path-component child))))))
      (funcall function child (unparse-string-template child-path))
      (walk-nodes function child child-path))))

(defun param-capture-children-p (node)
  "Predicate, returns nil if none of the node children can perform parameter capture"
  (remove-if-not #'keywordp (hash-table-keys (node-children node))))

(defun param-capture (node path-component &optional rest)
  "Given a node and a path component and optionally the rest of the path,
if the node can perform parameter capture return the child that matched and the captured parameters."
  (let ((integer-child (gethash :integer (node-children node)))
        (string-child (gethash :string (node-children node)))
        (wild-child (gethash :wild (node-children node))))
    (when (and integer-child (ppcre:scan "^[0-9]+$" path-component))
      (return-from param-capture (values integer-child (cons (car (node-path-component integer-child)) (parse-integer path-component)))))
    (when string-child
      (return-from param-capture (values string-child (cons (car (node-path-component string-child)) path-component))))
    (when wild-child
      (return-from param-capture (values wild-child (cons :* (str:join "/" (cons path-component rest))))))))

(defun find-child (node path-component &optional rest)
  "Find the child in node, if the path is not found, attempt to capture parameters"
  (let ((child (gethash path-component (node-children node))))
    (cond (child (values child nil))
          ((param-capture-children-p node) (param-capture node path-component rest))
          (t (values nil nil)))))

(defun find-path (root path)
  "Find the node-route of the node designated by path."
  (bind (((:labels find-path-internal (node path-component rest params))
          (bind (((:values child p) (find-child node path-component rest))
                 (params (if p (cons p params) params)))
            (cond ((and child rest)
                   (if (assoc :* params)
                       (values (car (node-route child)) params)
                       (find-path-internal child (car rest) (cdr rest) params)))
                  (child (values (car (node-route child)) params))
                  (t (values nil params)))))
         (path (parse-string-template (string-left-trim "/" path))))
    (if (equal '("") path)
        (values (car (node-route root)) nil)
        (find-path-internal root (car path) (cdr path) nil))))

;; High-level API

(defparameter *router* nil)

(defclass router ()
  ((root :accessor router-root-node
         :initarg :root
         :initform (make-instance 'node :path-component "")
         :type node
         :documentation "Root node of the router route tree")
   (route-map :accessor router-route-map
              :initform (make-hash-table)
              :type hash-table
              :documentation "Map of routes for easier debugging")
   (route-name-map :accessor router-route-name-map
                   :initform (make-hash-table)
                   :type hash-table
                   :documentation "Reverse lookup map"))
  (:documentation "Map of routes."))

(defun rebuild-route-map (router)
  "Rebuild an router's reverse lookup maps."
  ;; Clear the route maps
  (setf (router-route-name-map router) (make-hash-table))
  (setf (router-route-map router) (make-hash-table))
  ;; walk-node visits every child, we check if the child has a route assosiated with it and put it in the maps.
  (if (node-route (router-root-node router))
      (bind (((route-handler . route-name) (node-route (router-root-node router))))
        (when (and route-name (not (member route-name (hash-table-keys (router-route-name-map router)))))
          (setf (gethash route-name (router-route-name-map router)) "/"))
        (when route-handler
          (setf (gethash "/" (router-route-map router)) (list route-handler route-name)))))
  (walk-nodes
   (lambda (child path)
     (when (node-route child)
       (bind (((route-handler . route-name) (node-route child)))
         (when (and route-name (not (member route-name (hash-table-keys (router-route-name-map router)))))
           (setf (gethash route-name (router-route-name-map router)) path))
         (when route-handler
           (setf (gethash path (router-route-map router)) (list route-handler route-name))))))
   (router-root-node router)))

(defun find-route (uri &key (router *router*))
  "Find a route in router"
  (find-path (router-root-node router) uri))

(defun find-route-uri (route-name &key (router *router*) (params nil))
  "Attempt to find a uri that will match a given route in router given a route-name.
Because the route might have variable capture keys on it's path, you must provide a plist of
apropriate variable capture values in order to build a valid path."
  (when (gethash route-name (router-route-name-map router))
      (str:join "/"
                (iter (for path-component in (parse-string-template (gethash route-name (router-route-name-map router))))
                  (collect (if (stringp path-component)
                               path-component
                               (format nil "~A" (getf params (car path-component)))))))))

(defgeneric connect (router uri handler &optional name rebuild)
  (:documentation "Connect a route in the router. Optional rebuild parameter rebuilds the routeers reverse route map.
If nil, you will have to manually call REBUILD-ROUTE-MAP for reverse lookup to work correctly.")
  (:method ((router router) uri handler &optional (name nil) (rebuild nil))
    (add-route-at-path (router-root-node router) uri handler name)
    (when rebuild
      (rebuild-route-map router))
    router))

(defgeneric mount (router uri subrouter &optional route-name-prefix rebuild)
  (:documentation "Mount a subrouter to a URI in ROUTER. Optionally you can provide a route-name-prefix,
this must be an uppercase string, such as \"PREFIX-\" that will be prepended to the names of all
route-name keywords in the subrouter.
Optional rebuild parameter rebuilds the routeers reverse route map.
If nil, you will have to manually call REBUILD-ROUTE-MAP for reverse lookup to work correctly.
")
  (:method ((router router) uri (subrouter router) &optional (route-name-prefix nil) (rebuild nil))
    (let ((new-subrouter-node (merge-node-at-path (router-root-node router)
                                                  uri
                                                  (router-root-node subrouter))))
      (when route-name-prefix
        (walk-nodes (lambda (child path)
                      (declare (ignore path))
                      (when (node-route child)
                        (bind (((_ . route-name) (node-route child)))
                          (if route-name
                              (setf (node-route child)
                                    (cons route-name
                                          (make-keyword (str:concat route-name-prefix
                                                                    (symbol-name route-name)))))))))
                    new-subrouter-node))
      (when rebuild
        (rebuild-route-map router))
      router)))

(defgeneric route-map-pretty (router)
  (:documentation "Return a solted alist of the route-map for debugging")
  (:method ((router router))
    (sort (alexandria:hash-table-alist (router-route-map router))
          #'string<
          :key #'car)))
