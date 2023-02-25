;;;;    handler.lisp
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

(defpackage #:nite.handler
  (:use #:cl #:iterate)
  (:import-from #:bind
                #:bind)
  (:import-from #:closer-mop
                #:funcallable-standard-class
                #:funcallable-standard-object
                #:set-funcallable-instance-function)
  (:import-from #:media-types
                #:media-subtypep)
  (:import-from #:nite.request
                #:request-accepts-p)
  (:export
   #:define-handler
   #:define-handler-set
   #:handler
   #:handler-predicate
   #:handler-function
   #:handler-set
   #:handler-set-handlers))

(in-package #:nite.handler)

(defun make-request-predicate (&key (method nil) (content-type nil) (accepted-type nil) (additional-predicates nil))
  "Returns a function of one argument REQUEST of type LACK.REQUEST:REQUEST. Function returns a generalized boolean if
the  request satisfies certain conditions.
Keyword arguments:
 - :method (default nil) keyword
Used to match the request-method. if nil, all request methods are matched.
 - :content-type (default nil) keyword or string designating a valid media-type.
Used to match against the request content-type. if nil, all requests are matched.
 - :accepted-type (default nil) keyword or string designating a valid media-type
Used to match against the request accepted types. if nil, all requests are matched.
 - :additional-predicates (default nil) a list of functions of one argument REQUEST, returning a boolean.
In order for the request-predicate to return true, all additional-predicate functions must return true.
"
  (lambda (request)
    (and (if method
             (equal method (getf request :request-method))
             t)
         (if content-type
             (media-subtypep content-type (getf request :content-type))
             t)
         (if accepted-type
             (request-accepts-p request accepted-type)
             t)
         (iter (for predicate in additional-predicates)
               (always (funcall predicate request))))))

(defclass handler (funcallable-standard-object)
  ((predicate-function :accessor handler-predicate
                       :initarg :predicate
                       :initform nil
                       :type (or null function)
                       :documentation "Function of one argument REQUEST. Must return true for the handler to match.")
   (handler-function :accessor handler-function
                     :initarg :handler-function
                     :type function
                     :documentation "
Handler function. Get's called by (APPLY handler-function env) where env is the plist resperenting the request."))
  (:documentation "
Funcallable object. Takes one argument REQUEST, and if the request satisfies the handler's predicates, handles the
request and returns a valid response. Returns nil otherwise.")
  (:metaclass funcallable-standard-class))

(defmethod initialize-instance :after ((handler handler) &key)
  (set-funcallable-instance-function
   handler
   (lambda (request)
     (when (funcall (handler-predicate handler) request)
       (apply (handler-function handler) request)))))

(defclass handler-set (funcallable-standard-object)
  ((handlers :accessor handler-set-handlers
             :initarg :handlers
             :initform nil
             :type (or null list)
             :documentation "List of handlers in the handler set"))
  (:documentation "Funcallable object containing a set of related handlers.")
  (:metaclass funcallable-standard-class))

(defgeneric find-matching-handler (handler-set request)
  (:documentation "Find a handler in the handler-set that matches the request")
  (:method ((handler-set handler-set) request)
    (iter (for handler in (handler-set-handlers handler-set))
          (finding handler
                   such-that
                   #'(lambda (handler)
                       (funcall (handler-predicate handler) request))))))

(defmethod initialize-instance :after ((handler-set handler-set) &key)
  (set-funcallable-instance-function
   handler-set
   (lambda (request)
     (let ((handler (find-matching-handler handler-set request)))
       (when handler
         (apply (handler-function handler) request))))))

(defmacro define-handler (name
                          (&rest predicate-args)
                          (&rest args)
                          &body body)
  "Define a handler function.
predicate-args should be same as the arguments to make-request-predicate.
args should be valid keys in the request environment."
  (bind ((documentation (if (stringp (car body)) (car body) nil))
         (new-body (if (stringp (car body)) (cdr body) body)))
    `(progn
       (setf (fdefinition ',name)
             (make-instance 'handler
                            :predicate (make-request-predicate ,@predicate-args)
                            :handler-function (lambda (&key ,@args &allow-other-keys)
                                                (block ,name
                                                  ,@new-body))))
       ,(when documentation
          `(setf (documentation ',name 'function) ,documentation)))))

(defmacro define-handler-set (name &body handler-definitions)
  "Define a handler-set function.
handler-definitions have the following form: ((predicate-args) (args) &body) similar to define-handler."
  (bind ((documentation (if (stringp (car handler-definitions)) (car handler-definitions) nil))
         (definitions (if (stringp (car handler-definitions)) (cdr handler-definitions) handler-definitions)))
    `(progn
       (setf (fdefinition ',name)
             (make-instance 'handler-set))
       ,@(iter (for (predicate-args args . handler-body) in definitions)
               (collect `(push (make-instance 'handler
                                              :predicate (make-request-predicate ,@predicate-args)
                                              :handler-function (lambda (&key ,@args &allow-other-keys)
                                                                  ,@handler-body))
                               (handler-set-handlers (fdefinition ',name)))))
       ;; We want the handlers to be in the order the were defined in.
       (setf (handler-set-handlers (fdefinition ',name))
             (nreverse (handler-set-handlers (fdefinition ',name))))
       ,(when documentation
          `(setf (documentation ',name 'function) ,documentation)))))
