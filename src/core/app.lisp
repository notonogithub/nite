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
                #:*router*
                #:find-route
                #:find-route-uri
                #:connect
                #:mount
                #:rebuild-route-map
                )
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
   #:app
   #:*request*))

(in-package #:nite.app)

(defparameter *request* nil "Current handled request. Instance of lack.request:request")

(defclass app (funcallable-standard-object router)
  ((name :accessor app-name
         :initarg :name
         :type symbol
         :documentation "name of the app"))
  (:documentation "Funcallable object intended to be used as a clack application. Maintains it's own route table.")
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((app app) &key)
  (set-funcallable-instance-function
   app
   (lambda (env)
     (bind ((*router* app)
            ((:values route-handler route-parameters) (find-route (getf env :path-info)))
            (*request* (make-request env :route-parameters route-parameters)))
       (if route-handler
           (or (funcall route-handler *request*)
               (not-found)) ;; Eventually handlers should use the condition system to report HTTP errors instead of returning nil.
           (not-found :uri (getf env :path-info)))))))

(defmacro with-app (app &body body)
  "Add routes and subrouters specified in body. Body clauses must be of the form:
(:uri <template> <route-handler> &optional <route-tag>) or
(:router <template> <subrouter> &optional <prefix>)
Returns the app with updated routes. Rebuilds the route map automatically."
  (let ((app-name (gensym "APP")))
    `(let* ((,app-name ,app))
       ,@(iter (for (clause . arguments) in body)
               (cond ((eql clause :uri)
                      (collect `(connect ,app-name ,@arguments)))
                     ((eql clause :router)
                      (collect `(mount ,app-name ,@arguments)))
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
