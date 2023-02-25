;;;;    response.lisp
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

(defpackage #:nite.response
  (:use #:cl)
  (:export
   #:*response-headers*
   #:*not-found-message*
   #:response
   #:redirect
   #:not-found))

(in-package #:nite.response)

(defvar *response-headers* nil)

(defvar *not-found-message* "Not Found")

(defun response (content &key (status 200) (headers nil))
  (list status (concatenate 'list headers *response-headers*) (if content (list content))))

(defun redirect (destination &key (status 302))
  "Redirect response. By default uses status 302."
  (response nil :status status :headers (list :location destination)))

(defun not-found (&key uri message)
  (let ((content (if message
                     message
                     (if uri
                         (format nil "URI not found: ~A" uri)
                         *not-found-message*))))
    (response content
              :status 404
              :headers (list :content-type "text/plain" :content-length (length content)))))
