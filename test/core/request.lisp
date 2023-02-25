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


(defpackage #:nite.test.request
  (:use #:cl
        #:nite.request
        #:parachute)
  (:import-from #:bind #:bind)
  (:export
   #:test-request))

(in-package #:nite.test.request)

(defparameter *dummy-env*
  (list :request-method :post
        :script-name ""
        :path-info ""
        :url-scheme "http"
        :server-name "localhost"
        :server-port 8080
        :server-protocol :http/1.1
        :request-uri "/"
        :remote-addr "127.0.0.1"
        :remote-port 8080
        :headers (alexandria:alist-hash-table
                  '(("accept" . "application/json")
                    ("referer" . "http://some-made-up.site")
                    ("user-agent" . "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:93.0) Gecko/20100101 Firefox/93.0")
                    ("cookie" . "foo=bar;baz=quix"))
                  :test 'equal)
        :query-string "hello=world&foo=bar"
        :raw-body (flexi-streams:make-flexi-stream
                   (flexi-streams:make-in-memory-input-stream
                    (flexi-streams:string-to-octets "{\"key\": \"value\"}" :external-format :utf8)))
        :content-type "application/json"
        :content-length 16))

(define-test test-request
  :parent 'nite.test:nite-test
  (let ((request (make-request *dummy-env* :route-parameters nil)))
    (is equal (getf request :cookies) '("foo" "bar" "baz" "quix"))
    (is equal (getf request :query-parameters) '("hello" "world" "foo" "bar"))
    (is equal (getf request :accepted-types) '("application/json"))
    (is equal (alexandria:hash-table-plist (funcall (getf request :body))) '("key" "value"))))
