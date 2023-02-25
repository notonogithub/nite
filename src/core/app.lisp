;;;;    app.lisp
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

(defpackage #:nite.app
  (:use #:cl #:iterate)
  (:import-from #:bind
                #:bind)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:nite.router
                #:router
                #:router-children
                #:router-route-tag
                #:router-route-handler
                #:router-param-name
                #:router-add-route-handler
                #:router-add-subrouter
                #:unparse-string-template
                #:walk-router
                #:find-route)
  (:import-from #:closer-mop
                #:funcallable-standard-class
                #:funcallable-standard-object
                #:set-funcallable-instance-function)
  (:import-from #:nite.request
                #:make-request)
  (:import-from #:nite.response
                #:not-found)
  (:export
   #:define-app
   #:with-app
   #:find-route-uri
   #:app
   #:*app*
   #:*request*
   #:*debug*))

(in-package #:nite.app)

(defparameter *request* nil "Current handled request. Instance of lack.request:request")
(defparameter *app* nil "Current active app")
(defparameter *debug* nil "Global debug flag.")

(defclass app (funcallable-standard-object router)
  ((name :accessor app-name
         :initarg :name
         :type symbol
         :documentation "name of the app")
   (route-tag-map  :accessor app-route-tag-map
                   :initform (make-hash-table)
                   :type hash-table
                   :documentation "reverse-lookup table of routes by tag")
   (route-map :accessor app-route-map
              :initform (make-hash-table)
              :type hash-table
              :documentation "Map of routes for easier debugging"))
  (:documentation "Funcallable object intended to be used as a clack application. Maintains it's own route table.")
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((app app) &key)
  (set-funcallable-instance-function
   app
   (lambda (env)
     (bind ((*app* app)
            ((:values route-handler route-parameters) (find-route app (getf env :path-info)))
            (*request* (make-request env :route-parameters route-parameters)))
       (if route-handler
           (or (funcall route-handler *request*)
               (not-found)) ;; Eventually handlers should use the condition system to report HTTP errors instead of returning nil.
           (not-found :uri (getf env :path-info)))))))

(defun rebuild-route-map (app)
  "Rebuild an app's reverse lookup maps."
  ;; Clear the route maps
  (setf (app-route-tag-map app) (make-hash-table))
  (setf (app-route-map app) (make-hash-table))
  ;; walk-router visits every child, we check if the child has a route assosiated with it and put it in the maps.
  (walk-router
   app
   (list "")
   (lambda (path child)
     (let ((string-path (unparse-string-template path))
           (route-tag (router-route-tag child))
           (route-handler (router-route-handler child)))
       (when (and route-tag (not (member route-tag (hash-table-keys (app-route-tag-map app)))))
         (setf (gethash (router-route-tag child) (app-route-tag-map app)) path))
       (when route-handler
         (setf (gethash string-path (app-route-map app)) (if route-tag
                                                             (list route-handler route-tag)
                                                             route-handler)))))))

(defun find-route-uri (route-tag &key (app *app*) (params nil))
  "Attempt to find a uri that will match a given route in app given a route-tag.
Because the route might have variable capture keys on it's path, you must provide apropriate variable capture values in
the URI in order to build a valid path."
  (str:join "/"
            (iter (for path-component in (gethash route-tag (app-route-tag-map app)))
                  (collect (if (stringp path-component)
                               path-component
                               (format nil "~A" (getf params (car path-component))))))))

(defmacro with-app (app &body body)
  "Add routes and subrouters specified in body. Body clauses must be of the form:
(:uri <template> <route-handler> &optional <route-tag>) or
(:router <template> <subrouter>)
Returns the app with updated routes. Rebuilds the route map automatically."
  (let ((app-name (gensym "APP")))
    `(let* ((,app-name ,app))
       ,@(iter (for (clause . arguments) in body)
               (cond ((eql clause :uri)
                      (collect `(router-add-route-handler ,app-name ,@arguments)))
                     ((eql clause :router)
                      (collect `(router-add-subrouter ,app-name ,@arguments)))
                     (t nil)))
       (rebuild-route-map ,app-name)
       ,app-name)))

(defmacro define-app (name (&key (app-class 'app)) &body body)
  "Defines a new named-router with routes and subrouters specified in the body.
Takes a keyword parameter to specify a router class.
Body clauses must be of the form:
(:uri <template> <route-handler> &optional <route-tag>) or
(:router <template> <subrouter>)
Returns the new router"
  (let ((app-gensym (gensym "APP")))
    `(let ((,app-gensym (make-instance ',app-class :name ',name)))
       (setf (fdefinition ',name)
             ,app-gensym)
       (with-app ,app-gensym
         ,@body))))

(defmethod print-object ((app app) stream)
  (print-unreadable-object (app stream)
    (if *debug*
        (format stream "App ~A:~%~{~A~%~}"
                (app-name app)
                (mapcar (lambda (line)
                          (format nil "\"~A\" -> ~A" (car line) (cdr line)))
                        (sort (alexandria:hash-table-alist (app-route-map app))
                              #'string<
                              :key #'car)))
        (format stream "App ~A" (app-name app)))))
