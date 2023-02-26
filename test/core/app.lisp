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

(defpackage #:nite.test.app
  (:use #:cl
        #:nite.app
        #:parachute)
  (:import-from #:bind #:bind)
  (:import-from #:nite.router
                #:find-route-uri
                #:*router*)
  (:export
   #:test-find-route-uri))

(in-package #:nite.test.app)

(define-test test-find-route-uri
  :parent 'nite.test:nite-test
  (define-app test1 ()
    (:uri "/hello/world" :hello-route :hello-route-tag))
  (is string= (find-route-uri :hello-route-tag :router #'test1) "/hello/world")
  (define-app test2 ()
    (:uri "/hello/:name/:id|integer" :hello-route :hello-route-tag))
  (is string= (find-route-uri :hello-route-tag :router #'test2 :params '(:id 11 :name "world")) "/hello/world/11")
  (let ((*router* #'test2))
    (is string= (find-route-uri :hello-route-tag :params '(:id 11 :name "world")) "/hello/world/11")))
