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


(defpackage #:nite.test.router
  (:use #:cl
        #:nite.router
        #:parachute)
  (:import-from #:bind #:bind)
  (:export
   #:test-param-parser
   #:test-parse-string-template
   #:test-concatenate-string-template
   #:test-router-set/get-child
   #:test-router-add-route-handler
   #:test-match-child
   #:test-find-route
   #:test-merge-routers
   #:test-router-add-subrouter
   #:test-merge-different-routers))

(in-package #:nite.test.router)

(define-test test-param-parser
  :parent 'nite.test:nite-test
  (true (assoc :string *param-parsers*))
  (true (assoc :integer *param-parsers*))
  (bind (((_ . parser) (assoc :string *param-parsers*)))
    (is eq (param-parser-type parser) :string)
    (is eq (param-parser-parse-function parser) 'identity)
    (true (funcall (param-parser-condition parser) "foo"))
    (false (funcall (param-parser-condition parser) "")))
  (bind (((_ . parser) (assoc :integer *param-parsers*)))
    (is eq (param-parser-type parser) :integer)
    (is eq (param-parser-parse-function parser) 'parse-integer)
    (true (funcall (param-parser-condition parser) "11"))
    (false (funcall (param-parser-condition parser) "foo"))
    (false (funcall (param-parser-condition parser) "11a"))
    (false (funcall (param-parser-condition parser) "11.1"))))

(define-test test-parse-string-template
  :parent 'nite.test:nite-test
  (true (tree-equal (parse-string-template "/hello/world")
                    (list "" "hello" "world")
                    :test 'equal))
  (true (tree-equal (parse-string-template "/hello/:name|string/:id|integer")
                    (list "" "hello" (list :name :string) (list :id :integer))
                    :test 'equal)))

(define-test test-concatenate-string-template
  :parent 'nite.test:nite-test
  (is string= (concatenate-string-template "" "") "/")
  (is string= (concatenate-string-template "/" "") "/")
  (is string= (concatenate-string-template "" "/") "/")
  (is string= (concatenate-string-template "/" "/") "/")
  (is string= (concatenate-string-template "" "hello") "/hello")
  (is string= (concatenate-string-template "hello" "") "/hello")
  (is string= (concatenate-string-template "/" "hello") "/hello")
  (is string= (concatenate-string-template "/" "/hello") "/hello")
  (is string= (concatenate-string-template "/" "/hello/") "/hello/")
  (is string= (concatenate-string-template "/" "hello/") "/hello/"))

(define-test test-router-set/get-child
  :parent 'nite.test:nite-test
  (bind ((router (make-instance 'router))
         (child (make-instance 'router)))
    (router-set-child router "hello" child)
    (is eq (router-get-child router "hello") child))
  (bind ((router (make-instance 'router))
         (child (make-instance 'param-router)))
    (router-set-child router :string child)
    (is eq (router-get-child router (list :hello :string)) child))
  (bind ((router (make-instance 'router))
         (child (make-instance 'param-router)))
    (router-set-child router (list :hello :string) child)
    (is eq (router-get-child router (list :hello :string)) child))
  (bind ((router (make-instance 'router))
         (child (router-get-child router "hello" :ensure t)))
    (is eq (router-get-child router "hello") child))
  (bind ((router (make-instance 'router))
         (child (router-get-child router (list :hello :string) :ensure t)))
    (is eq (router-get-child router (list :hello :string)) child)))

(define-test test-router-add-route-handler
  :parent 'nite.test:nite-test
  (bind ((router (make-instance 'router)))
    (router-add-route-handler router "/hello" :hello-route :hello-route-tag)
    (bind ((child (router-get-child router "hello")))
      (is equal (router-route-handler child) :hello-route)
      (is equal (router-route-tag child) :hello-route-tag)))
  (bind ((router (make-instance 'router)))
    (router-add-route-handler router "/hello/world" :hello-route :hello-route-tag)
    (bind ((child (router-get-child (router-get-child router "hello") "world")))
      (is equal (router-route-handler child) :hello-route)
      (is equal (router-route-tag child) :hello-route-tag)))
  (bind ((router (make-instance 'router)))
    (router-add-route-handler router "/:hello" :hello-route :hello-route-tag)
    (bind ((child (router-get-child router (list :hello :string))))
      (is equal (router-route-handler child) :hello-route)
      (is equal (router-route-tag child) :hello-route-tag)))
  (bind ((router (make-instance 'router)))
    (router-add-route-handler router "/hello/:world" :hello-route :hello-route-tag)
    (bind ((child (router-get-child (router-get-child router "hello") (list :world :string))))
      (is equal (router-route-handler child) :hello-route)
      (is equal (router-route-tag child) :hello-route-tag))))

(define-test test-find-route
  :parent 'nite.test:nite-test
  (bind ((router (make-instance 'router)))
    (router-add-route-handler router "/hello/:name/:id|integer" :hello-route)
    (bind (((:values route bindings) (find-route router "/hello/myname/11")))
      (is equal route :hello-route)
      (true (tree-equal bindings '((:id . 11) (:name . "myname")) :test 'equal)))))


;; This will need more testing.

(define-test test-merge-routers
  :parent 'nite.test:nite-test
  (bind ((router1 (make-instance 'router))
         (router2 (make-instance 'router)))
    (router-add-route-handler router1 "common" :route1 :route-tag1)
    (router-add-route-handler router1 "unique-to-router1" :route-unique-to-router1 :route-tag-unique-to-router1)
    (router-add-route-handler router2 "common" :route2 :route-tag2)
    (router-add-route-handler router2 "unique-to-router2" :route-unique-to-router2 :route-tag-unique-to-router2)
    (bind ((merged-router (merge-routers router1 router2))
           (common (router-get-child merged-router "common"))
           (unique-to-router1 (router-get-child merged-router "unique-to-router1"))
           (unique-to-router2 (router-get-child merged-router "unique-to-router2")))
      (is equal (router-route-handler common) :route2)
      (is equal (router-route-tag common) :route-tag2)
      (is equal (router-route-handler unique-to-router1) :route-unique-to-router1)
      (is equal (router-route-tag unique-to-router1) :route-tag-unique-to-router1)
      (is equal (router-route-handler unique-to-router2) :route-unique-to-router2)
      (is equal (router-route-tag unique-to-router2) :route-tag-unique-to-router2))))

(define-test test-merge-different-routers
  :parent 'nite.test:nite-test
  (bind ((router1 (make-instance 'param-router :param-name :name))
         (router2 (make-instance 'router)))
    (router-add-route-handler router1 "common" :route1 :route-tag1)
    (router-add-route-handler router1 "unique-to-router1" :route-unique-to-router1 :route-tag-unique-to-router1)
    (router-add-route-handler router2 "common" :route2 :route-tag2)
    (router-add-route-handler router2 "unique-to-router2" :route-unique-to-router2 :route-tag-unique-to-router2)
    (bind ((merged-router (merge-routers router1 router2))
           (common (router-get-child merged-router "common"))
           (unique-to-router1 (router-get-child merged-router "unique-to-router1"))
           (unique-to-router2 (router-get-child merged-router "unique-to-router2")))
      (is equal (type-of merged-router) 'param-router)
      (is equal (router-param-name merged-router) :name)
      (is equal (router-route-handler common) :route2)
      (is equal (router-route-tag common) :route-tag2)
      (is equal (router-route-handler unique-to-router1) :route-unique-to-router1)
      (is equal (router-route-tag unique-to-router1) :route-tag-unique-to-router1)
      (is equal (router-route-handler unique-to-router2) :route-unique-to-router2)
      (is equal (router-route-tag unique-to-router2) :route-tag-unique-to-router2)))
  ;; opposite case
  (bind ((router1 (make-instance 'router))
         (router2 (make-instance 'param-router :param-name :name)))
    (router-add-route-handler router1 "common" :route1 :route-tag1)
    (router-add-route-handler router1 "unique-to-router1" :route-unique-to-router1 :route-tag-unique-to-router1)
    (router-add-route-handler router2 "common" :route2 :route-tag2)
    (router-add-route-handler router2 "unique-to-router2" :route-unique-to-router2 :route-tag-unique-to-router2)
    (bind ((merged-router (merge-routers router1 router2))
           (common (router-get-child merged-router "common"))
           (unique-to-router1 (router-get-child merged-router "unique-to-router1"))
           (unique-to-router2 (router-get-child merged-router "unique-to-router2")))
      (is equal (type-of merged-router) 'param-router)
      (is equal (router-param-name merged-router) :name)
      (is equal (router-route-handler common) :route2)
      (is equal (router-route-tag common) :route-tag2)
      (is equal (router-route-handler unique-to-router1) :route-unique-to-router1)
      (is equal (router-route-tag unique-to-router1) :route-tag-unique-to-router1)
      (is equal (router-route-handler unique-to-router2) :route-unique-to-router2)
      (is equal (router-route-tag unique-to-router2) :route-tag-unique-to-router2))))


(define-test test-router-add-subrouter :parent 'nite.test:nite-test
  (bind ((router (make-instance 'router))
         (subrouter (make-instance 'router)))
    (router-add-route-handler subrouter "world" :route1)
    (router-add-subrouter router "hello" subrouter)
    (bind ((child-in-router (router-get-child (router-get-child router "hello") "world"))
           (child-in-subrouter (router-get-child subrouter "world")))
      (is equal (router-route-handler child-in-router) (router-route-handler child-in-subrouter)))))
