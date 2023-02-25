;;;;    request.lisp
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

(defpackage #:nite.request
  (:use #:cl #:iterate)
  (:import-from #:bind
                #:bind)
  (:import-from #:alexandria
                #:alist-plist
                #:read-stream-content-into-string
                #:make-keyword)
  (:import-from #:quri
                #:url-decode-params)
  (:import-from #:ppcre
                #:split)
  (:import-from #:rfc2388
                #:parse-mime)
  (:import-from #:flexi-streams
                #:flexi-stream)
  (:import-from #:media-types
                #:media-type
                #:media-subtypep
                #:media-type-values
                #:parse-media-type)
  (:export
   #:request-accepts-p
   #:make-request
   #:parse-stream-according-to-media-type
   #:parse-query-parameters
   #:parse-cookies
   #:parse-accepted-types))

(in-package #:nite.request)


(defun parse-query-parameters (request)
  "Parse the query parameters of the request"
  (alist-plist (url-decode-params (or (getf request :query-string)
                                      "") :lenient t)))

(defun parse-cookies (request)
  "Parse the cookie header of the request"
  (alist-plist (iter (for cookie-param in (split "\\s*[,;]\\s*" (gethash "cookie" (getf request :headers) "")))
                     (appending (url-decode-params cookie-param :lenient t)))))

(defun parse-accepted-types (request)
  "Parse the accept header of the request"
  (split "\\s*[,]\\s*" (gethash "accept" (getf request :headers) "")))

(defun request-accepts-p (request media-type)
  "Match media-type against the accept header of the request"
  (member media-type (parse-accepted-types request) :test #'media-subtypep))

(defgeneric parse-stream-according-to-media-type (stream parser &key content-length &allow-other-keys)
  (:documentation "parse content of stream using media-type/parser")
  (:method :after ((stream flexi-stream) parser &key content-length &allow-other-keys)
    (declare (ignore content-length))
    (file-position stream 0))
  (:method (stream parser &key (content-length 0) &allow-other-keys)
    (if content-length
        (read-stream-content-into-string
         stream
         :buffer-size content-length)
        ""))
  (:method (stream (parser (eql :|application/x-www-form-urlencoded|)) &key content-length &allow-other-keys)
    (alist-plist
     (url-decode-params
      (read-stream-content-into-string
       stream
       :buffer-size content-length)
      :lenient t)))
  (:method (stream (parser (eql :|multipart/form-data|)) &key (content-length nil) boundry &allow-other-keys)
    (declare (ignore content-length))
    (parse-mime stream boundry))
  (:method (stream (parser (eql :|application/json|))  &key (content-length nil) &allow-other-keys)
    (declare (ignore content-length))
    (com.inuoe.jzon:parse stream :allow-comments t :allow-trailing-comma t))
  (:method (stream (parser media-type)  &key (content-length nil) &allow-other-keys)
    (bind (((:values type subtype props) (media-type-values parser)))
      (apply #'parse-stream-according-to-media-type
             stream
             (make-keyword (format nil "~a/~a" type subtype))
             :content-length content-length
             (alist-plist props))))
  (:method (stream (parser string) &key (content-length nil) &allow-other-keys)
    (parse-stream-according-to-media-type stream (parse-media-type parser) :content-length content-length)))

(defun make-request (request &key (route-parameters nil))
  "Augment the request with additional info."
  (list* :route-parameters route-parameters
         :cookies (parse-cookies request)
         :accepted-types (parse-accepted-types request)
         :query-parameters (parse-query-parameters request)
         ;; We want the body to be parsed lazily
         :body (lambda () (parse-stream-according-to-media-type (getf request :raw-body)
                                                                (getf request :content-type)
                                                                :content-length (getf request :content-length)))
         request))
