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

(defpackage #:nite.router
  (:use #:cl #:iterate)
  (:import-from #:bind
                #:bind)
  (:import-from #:alexandria
                #:hash-table-keys
                #:make-keyword)
  (:export
   ;; Templates
   #:*param-parsers*
   #:param-parser
   #:param-parser-type
   #:param-parser-parse-function
   #:param-parser-condition
   #:register-param-parser
   #:parse-string-template
   #:unparse-string-template
   #:concatenate-string-template
   ;; Router classes
   #:router
   #:param-router
   ;; Router slot accessors
   #:router-children
   #:router-route-handler
   #:router-route-tag
   #:router-param-name
   #:router-name
   ;; Low-level router operations
   #:router-set-child
   #:router-get-child
   #:router-add-route-handler
   #:merge-routers
   #:router-add-subrouter
   ;; Router API
   #:find-route
   #:walk-router))

(in-package #:nite.router)

;; Templates and parameter parsing.

(defparameter *param-parsers* nil
  "registry of param-parsers. alist, should be sorted in order of specificity.
For example integer is more specific than string, and should appear before it.")

(defclass param-parser ()
  ((parser-type
    :reader param-parser-type
    :initarg :type
    :type keyword
    :documentation "keyword naming the param-parser type. Will appear in templates as \"<name>|<type>.")
   (parse-function
    :reader param-parser-parse-function
    :initarg :parse-function
    :type (or function symbol)
    :documentation "Function. Takes one string argument and transforms it in some way.")
   (condition
    :reader param-parser-condition
    :initarg :condition
    :type (or function symbol)
    :documentation "Function. Takes one string argument and tests if it's a valid value for the parse-function."))
  (:documentation "param parser specifies how parameter capture variables are to be parsed.
In string templates a parameter capture variable is defined by the form :<name>|<type>
where the <name> is the name of the variable and the <type> optional(default is :string).
Type must be a registered param-parser type. By default :string and :integer are provided.
An invalid type will default to string.
A param-parser has two functions the condition function tests the path component if it's parsable,
the parse-function parses it."))

(defun register-param-parser (&key type parse-function condition)
  "Register a param-parser and adds it to *param-parsers*. Arguments
:type the type of the param-parser, should be a keyword.
:parse-function a function of one string argument that returns a parsed object.
:condition a bool function of one string argument. Should test if the path component is parsable by the parse-function"
  (push (cons type (make-instance 'param-parser
                                  :type type
                                  :parse-function parse-function
                                  :condition condition))
        *param-parsers*))

;; Default param parsers

(register-param-parser :type :string
                       :parse-function 'identity
                       :condition (lambda (param) (ppcre:scan "^.+$" param)))

(register-param-parser :type :integer
                       :parse-function 'parse-integer
                       :condition (lambda (param) (ppcre:scan "^[0-9]+$" param)))

(defun parse-string-template (string-template)
  "Parses a string URI template to a list of path components. Input can include parameter capture variables.
Example:
  \"/foo/bar\" -> (\"\" \"foo\" \"bar\")
  \"/foo/:name|string/:id|integer\" -> (\"\" \"foo\" (:name :string) (:id :integer))
  \"/foo/:name/:id|integer\" -> (\"\" \"foo\" (:name :string) (:id :integer))"
  (bind (((:labels parse-param-capture (path-component))
          (bind (((name &optional (parser-name "string")) (str:split "|" path-component))
                 (name (make-keyword (str:upcase (string-left-trim ":" name))))
                 (parser-name (make-keyword (str:upcase parser-name))))
            (list name (if (assoc parser-name *param-parsers*) parser-name :string)))))
    (iter (for component in (str:split "/" string-template))
          (if (str:starts-with-p ":" component)
              (collect (parse-param-capture component))
              (collect component)))))

(defun unparse-string-template (list-template)
  "Generate a string template from a parsed list template"
  (str:join "/" (iter (for path-component in list-template)
                      (collect (if (stringp path-component)
                                   path-component
                                   (apply 'format nil ":~A|~A" path-component))))))

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

;; Router

(defclass router ()
  ((children
    :accessor router-children
    :initform (make-hash-table :test #'equalp)
    :type hash-table
    :documentation "equalp hash-table containing the children routers. Keys are either a literal path-component
or a keyword denoting a param-parser type used for parameter capture.")
   (route-handler
    :accessor router-route-handler
    :initarg :route-handler
    :initform nil
    :documentation "Leaf of the router tree. Can be any arbitrary object.
It is assumed that this would be some sort of route handler in a web-framework context.")
   (route-tag
    :accessor router-route-tag
    :initarg :route-tag
    :initform nil
    :type (or null keyword)
    :documentation "(Optional) Route tag is a keyword used for reverse lookup.
Instead of searching the tree by matching a path, search the tree for a specific route-tag
and return it's route-handler"))
  (:documentation "A router contains information about URI paths. A router is a tree node representing the URI routes.
Each router is denoted by a single path component and contains a table of it's children with the child path components
as the keys.
Each router can contain a route-handler object, that is a leaf of the tree.
Optionally they can have a route-tag keyword attatched
The route-tag can be used for reverse lookup to find a path. This is useful for things like redirects."))

(defclass param-router (router)
  ((param-name
    :accessor router-param-name
    :initarg :param-name
    :type keyword
    :documentation "Keyword. Name of param capture variable."))
  (:documentation "param-router is a subclass of router that includes a param-name slot,
used in the route tree for param capture. While a regular router stands in the tree for a literal path-component,
the param-router stands for a variable-capture component that will do param-parsing during search."))

(defgeneric router-set-child (router path-spec child)
  (:documentation "Set a child in the router child table. path-spec must be a string denoting a path-component,
a keyword denoting a param-parser or a list denoting a variable capture path-component")
  (:method ((router router) (path-spec string) (child router))
    "Set a child in the router child table, path-spec must be a string denoting a path-component"
    (setf (gethash path-spec (router-children router)) child))
  (:method ((router router) (param-capture cons) (child router))
    "Set a child in the router child table, param-capture must be a list denoting a variable capture path-component"
    (router-set-child router (second param-capture) child))
  (:method ((router router) (param-parser-type symbol) (child router))
    "Set a child in the router child table, param-parser-type must be a keyword denoting a registered param-parser"
    (setf (gethash param-parser-type (router-children router)) child)))

(defgeneric router-get-child (router path-spec &key ensure)
  (:documentation "Get a child from the router child table denoted by path-spec. Path-spec must be a string denoting a
path-component or a list denoting a variable capture path-component.
Optional ensure argument creates an empty child router of the apropriate type if one is not found.")
  (:method ((router router) (path-spec string) &key (ensure nil))
    "Get a child from the router child table denoted by path-spec. Path-spec must be a string denoting a path-component
Optional ensure keyword argument creates an empty child router if one is not found."
    (or (gethash path-spec (router-children router))
        (when ensure
          (router-set-child router path-spec (make-instance 'router)))))
  (:method ((router router) (param-type symbol) &key ensure)
    "Get a child from the router child table denoted by param-capture.
param-capture must be a list denoting a variable capture path-component.
Optional ensure keyword argument does nothing in this case because there is no information about the param-name."
    (declare (ignore ensure))
    (gethash param-type (router-children router)))
  (:method ((router router) (param-capture cons) &key (ensure nil))
    "Get a child from the router child table denoted by param-capture.
param-capture must be a list denoting a variable capture path-component.
Optional ensure keyword argument creates an empty child param-router and sets it's param name if one is not found."
    (let ((param-name (first param-capture))
          (param-type (second param-capture)))
      (or (gethash param-type (router-children router))
          (when ensure
            (router-set-child router param-capture (make-instance 'param-router :param-name param-name)))))))

(defun router-add-route-handler (router template route-handler &optional (route-tag nil))
  "Add route-handler to a router. template can be any valid URI template, either a string or a parsed template.
router-add-route-handler will automatically create child routers for each path component if they do not exist.
route-handler can be any object stored as a leaf in the tree.
route-tag is an optional parameter used uniquely identify a route"
  (if (stringp template)
      (router-add-route-handler router
                                (parse-string-template (string-left-trim "/" template))
                                route-handler
                                route-tag)
      (bind (((head . tail) template)
             (child (router-get-child router head :ensure t)))
        (if (null tail)
            (setf (router-route-tag child) route-tag
                  (router-route-handler child) route-handler)
            (router-add-route-handler child tail route-handler route-tag)))))

(defun copy-children (new-router router1 router2)
  "Recursively copy the children of router1 and router2 in new-router."
  (let ((common-children (intersection (hash-table-keys (router-children router1))
                                       (hash-table-keys (router-children router2))))
        (router1-unique-children (set-difference (hash-table-keys (router-children router1))
                                                 (hash-table-keys (router-children router2))))
        (router2-unique-children (set-difference (hash-table-keys (router-children router2))
                                                 (hash-table-keys (router-children router1)))))
    (iter (for key in router1-unique-children)
          (setf (gethash key (router-children new-router))
                (gethash key (router-children router1))))
    (iter (for key in router2-unique-children)
          (setf (gethash key (router-children new-router))
                (gethash key (router-children router2))))
    (iter (for key in common-children)
          (setf (gethash key (router-children new-router))
                (merge-routers (gethash key (router-children router1))
                               (gethash key (router-children router2))))))
  new-router)

(defgeneric merge-routers (router1 router2)
  (:documentation "Merge two routers, result is a new router who's children are merged children of the source routers.
The algorithm is recursive and creates new routers and does not modify it's arguments.
In case of clashing values, values from router2 take priority and override the ones in router1.")
  (:method (router1 router2)
    (let ((new-router (make-instance 'router
                                     :route-handler (or (router-route-handler router2) (router-route-handler router1))
                                     :route-tag (or (router-route-tag router2) (router-route-tag router1)))))
      (copy-children new-router router1 router2)))
  (:method ((router1 param-router) router2)
    (let ((new-router (make-instance 'param-router
                                     :param-name (router-param-name router1)
                                     :route-handler (or (router-route-handler router2) (router-route-handler router1))
                                     :route-tag (or (router-route-tag router2) (router-route-tag router1)))))
      (copy-children new-router router1 router2)))
  (:method (router1 (router2 param-router))
    (let ((new-router (make-instance 'param-router
                                     :param-name (router-param-name router2)
                                     :route-handler (or (router-route-handler router2) (router-route-handler router1))
                                     :route-tag (or (router-route-tag router2) (router-route-tag router1)))))
      (copy-children new-router router1 router2))))

(defun router-add-subrouter (router template subrouter)
  "Add a subrouter to the router at the position denoted by the optional keyword argument template.
If a child already exists at that location the child will be recursively merged with subrouter.
If conflicting values are found, values from the subrouter will override the values in the preexisting child."
  (if (stringp template)
      (router-add-subrouter router (parse-string-template (string-left-trim "/" template))
                            subrouter)
      (bind (((head . tail) template)
             (child (router-get-child router head :ensure t)))
        (cond ((and (stringp head) (str:emptyp head)) (router-add-subrouter router tail subrouter))
              ((null tail) () (router-set-child router head (merge-routers child subrouter)))
              (t (router-add-subrouter child tail subrouter))))))

(defun find-route (router uri)
  "Find a route in the tree denoted by router given a uri, performing variable capture allong the way.
Returns two values, the route object and an alist of variable capture params."
  (bind (((:labels match-child (router path-component &optional (params nil)))
          (bind ((child (router-get-child router path-component)))
            (if child
                (values child params)
                (progn ;; a child was not found, so we search the param parser definitions for a possible match.
                  (iter (for (param-parser-type . param-parser) in *param-parsers*)
                        (let ((child (router-get-child router param-parser-type)))
                          (if (and child (funcall (param-parser-condition param-parser) path-component))
                              (return-from match-child
                                (values child
                                        ;; add the parsed parameter to the params alist
                                        (cons (cons (router-param-name child)
                                                    (funcall (param-parser-parse-function param-parser) path-component))
                                              params))))))
                  (values nil params)))))
         ((:labels find-route-internal (router uri &key (params nil)))
          (bind (((head . tail) uri))
            (if (and (str:emptyp head) tail)
                (find-route-internal router tail :params params)
                (bind (((:values child params) (match-child router head params)))
                  (cond ((and child tail) (find-route-internal child tail :params params))
                        ((and child (null tail)) (values (router-route-handler child) params))
                        (t (values nil params))))))))
    (find-route-internal router (if (stringp uri)
                                    (parse-string-template uri)
                                    uri))))

(defun walk-router (router path function)
  (iter (for (child-path-component child) in-hashtable (router-children router))
        (let* ((child-path (concatenate 'list path (if (stringp child-path-component)
                                                       (list child-path-component)
                                                       (list (list (router-param-name child)
                                                                   child-path-component))))))
          (funcall function child-path child)
          (walk-router child child-path function))))
