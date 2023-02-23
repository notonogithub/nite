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


(defpackage #:nite.test.handler
  (:use #:cl
        #:nite.handler
        #:parachute)
  (:import-from #:bind #:bind)
  (:import-from #:lack.request
                #:make-request)
  (:export
   #:test-handler
   #:test-handler-set))

(in-package #:nite.test.handler)

(defparameter *get*
  (list :request-method :get
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
                  '(("accept" . "text/html")
                    ("referer" . "http://some-made-up.site")
                    ("user-agent" . "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:93.0) Gecko/20100101 Firefox/93.0"))
                  :test 'equal)
        :content-type "text/html"))

(defparameter *post*
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
                  '(("accept" . "text/html")
                    ("referer" . "http://some-made-up.site")
                    ("user-agent" . "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:93.0) Gecko/20100101 Firefox/93.0")
                    ("cookie" . "foo=bar;baz=quix"))
                  :test 'equal)
        :query-string "hello=world&foo=bar"
        :content-type "text/html"))

(define-test test-handler
  :parent 'nite.test:nite-test
  (let ((request (make-request *get*)))
    (define-handler index (:method :get) ()
      '(200 nil ("hello")))
    (is equal (funcall 'index request) '(200 nil ("hello")))))

(define-test test-handler-set
  :parent 'nite.test:nite-test
  (let ((get-request (make-request *get*))
        (post-request (make-request *post*)))
    (define-handler-set form
      ((:method :get) ()
       '(200 nil ("hello")))
      ((:method :post) (query-parameters)
       (list 200 nil (list (format nil "~A" query-parameters)))))
    (is equal '(200 nil ("hello"))
        (funcall 'form get-request))
    (is equal '(200 nil ("((hello . world) (foo . bar))"))
        (funcall 'form post-request))))

