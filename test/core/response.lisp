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


(defpackage #:nite.test.response
  (:use #:cl
        #:nite.response
        #:parachute)
  (:import-from #:bind #:bind)
  (:export
   #:test-response))

(in-package #:nite.test.response)

(define-test test-response
  :parent 'nite.test:nite-test
  (is equal (response "Hello")
      '(200 nil ("Hello")))
  (is equal (redirect "/foo")
      '(302 (:location "/foo") nil))
  (is equal (not-found)
      '(404 (:content-type "text/plain" :content-length 9) ("Not Found"))))
