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
   #:test-parse-string-template
   #:test-unparse-string-template
   #:test-concatenate-string-template
   #:test-parameter-capture
   #:test-node-add-route-handler
   #:test-find-path
   #:test-merge-node-at-path
   #:test-find-route-uri
   #:test-mount
   #:test-pretty))

(in-package #:nite.test.router)

(define-test test-parse-string-template
  :parent 'nite.test:nite-test
  (true (tree-equal (parse-string-template "/hello/world")
                    (list "" "hello" "world")
                    :test 'equal))
  (true (tree-equal (parse-string-template "/hello/:name|string/:id|integer")
                    (list "" "hello" (cons :name :string) (cons :id :integer))
                    :test 'equal))
  (true (tree-equal (parse-string-template "/hello/*")
                    (list "" "hello" (cons :* :wild))
                    :test 'equal)))

(define-test test-unparse-string-template
  :parent 'nite.test:nite-test
  (is equal
      "/hello/world"
      (unparse-string-template (list "" "hello" "world")))
  (is equal
      "/hello/:NAME|STRING/:ID|INTEGER"
      (unparse-string-template (list "" "hello" (cons :name :string) (cons :id :integer))))
  (is equal
      "/hello/*"
      (unparse-string-template (list "" "hello" (cons :* :wild)))))

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

(define-test test-parameter-capture
  :parent 'nite.test:nite-test
  (let ((integer-child (make-instance 'node :path-component (cons :id :integer)))
        (string-child (make-instance 'node :path-component (cons :name :string)))
        (wild-child (make-instance 'node :path-component (cons :* :wild))))
    (let ((node (make-instance 'node)))
      (setf (gethash :integer (node-children node)) integer-child)
      (setf (gethash :string (node-children node)) string-child)
      (true (param-capture-children-p node))
      (false (param-capture-children-p (make-instance 'node)))
      (multiple-value-bind (child params)
          (param-capture node "foo")
        (is eq string-child child)
        (is equal (cons :name "foo") params))
      (multiple-value-bind (child params)
          (param-capture node "11")
        (is eq integer-child child)
        (is equal (cons :id 11) params)))
    (let ((node (make-instance 'node)))
      (setf (gethash :wild (node-children node)) wild-child)
      (multiple-value-bind (child params)
          (param-capture node "whatever" '("foo" "bar"))
        (is eq wild-child child)
        (is equal (cons :* "whatever/foo/bar") params)))))

(define-test test-node-add-route-handler
  :parent 'nite.test:nite-test
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/hello" 'hello-route :hello-route)
    (is equal
        (cons 'hello-route :hello-route)
        (node-route (gethash "hello" (node-children node)))))
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/hello/world" 'hello-route :hello-route)
    (is equal
        (cons 'hello-route :hello-route)
        (node-route
         (gethash "world"
                  (node-children
                   (gethash "hello" (node-children node)))))))
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/:hello" 'hello-route :hello-route)
    (is equal
        (cons 'hello-route :hello-route)
        (node-route (gethash :string (node-children node)))))
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/hello/:world" 'hello-route :hello-route)
    (is equal
        (cons 'hello-route :hello-route)
        (node-route
         (gethash :string
                  (node-children
                   (gethash "hello" (node-children node))))))))

(define-test test-find-path
  :parent 'nite.test:nite-test
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/" 'index :index)
    (bind (((:values route bindings) (find-path node "/")))
      (is equal route 'index)
      (false bindings)))
  (bind ((node (make-instance 'node :path-component "")))
    (add-route-at-path node "/hello/:name/:id|integer" 'hello-route :hello-route)
    (bind (((:values route bindings) (find-path node "/hello/myname/11")))
      (is equal route 'hello-route)
      (true (tree-equal bindings '((:id . 11) (:name . "myname")) :test 'equal)))))

(define-test test-merge-node-at-path
  :parent 'nite.test:nite-test
  (bind ((root (make-instance 'node :path-component ""))
         (child (make-instance 'node :path-component "")))
    (add-route-at-path child "/world" 'hello-route :hello-route)
    (merge-node-at-path root "/hello" child)
    (is equal
        (cons 'hello-route :hello-route)
        (node-route
         (gethash "world"
                  (node-children
                   (gethash "hello" (node-children root))))))
    (is equal
        "world"
        (node-path-component
         (gethash "world"
                  (node-children
                   (gethash "hello" (node-children root))))))))

;; Router

(define-test test-find-route-uri
  :parent 'nite.test:nite-test
  (let ((router (make-instance 'router)))
    (connect router "/hello" 'hello :hello t)
    (is equal
        "/hello"
        (find-route-uri :hello :router router))
    (connect router "/hello/:name/:id|integer" 'hello :hello-params t)
    (is equal
        "/hello/bob/11"
        (find-route-uri :hello-params :router router :params '(:name "bob" :id 11)))))

(define-test test-mount
  :parent 'nite.test:nite-test
  (let ((router (make-instance 'router))
        (child (make-instance 'router)))
    (connect child "/hello" 'hello :hello t)
    (connect child "/hello/:name/:id|integer" 'hello :hello-params t)
    (mount router "/blah" child "PREFIX-" t)
    (is equal
        "/blah/hello"
        (find-route-uri :prefix-hello :router router))
    (is equal
        "/blah/hello/bob/11"
        (find-route-uri :prefix-hello-params :router router :params '(:name "bob" :id 11)))))

(define-test test-pretty
  :parent 'nite.test:nite-test
  (let ((router (make-instance 'router)))
    (connect router "/" 'index :index)
    (connect router "/hello" 'hello :hello)
    (connect router "/hello/world" 'hello :hello-world)
    (connect router "/hello/bob" 'hello :hello-bob)
    (rebuild-route-map router)
    (is equal
        '(("/" index :index)
          ("/hello" hello :hello)
          ("/hello/bob" hello :hello-bob)
          ("/hello/world" hello :hello-world))
        (route-map-pretty router))))
