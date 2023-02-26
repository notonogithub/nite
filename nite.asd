;;;;    nite.asd
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

(asdf:defsystem #:nite
  :description "Common lisp web framework"
  :author "Pavel Penev (Lispegistus) <lispegistus@strangestack.com>"
  :license  "GPL-3.0"
  :version "0.2.0"
  :serial t
  :depends-on (#:alexandria
               #:serapeum
               #:iterate
               #:metabang-bind
               #:str
               #:quri
               #:com.inuoe.jzon
               #:shasht
               #:cl-ppcre
               #:rfc2388
               #:flexi-streams
               #:media-types
               #:clack
               #:closer-mop)
  :pathname "src"
  :components ((:module "core"
                :serial t
                :components ((:file "router")
                             (:file "request")
                             (:file "response")
                             (:file "handler")
                             (:file "app")
                             (:file "nite"))))
  :in-order-to ((asdf:test-op (asdf:test-op :nite/test))))

(asdf:defsystem #:nite/test
  :description "Test system for nite"
  :author "Pavel Penev (Lispegistus) <lispegistus@strangestack.com>"
  :license  "GPL-3.0"
  :version "0.2.0"
  :serial t
  :depends-on (#:nite
               #:parachute)
  :pathname "test"
  :components ((:file "nite-test") ;; Main entry point for running all test cases.
               (:module "core"
                :serial t
                :components ((:file "router")
                             (:file "request")
                             (:file "response")
                             (:file "handler")
                             (:file "app"))))
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :nite.test)))
