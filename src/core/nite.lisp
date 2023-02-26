;;;;    auth.lisp
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

(defpackage #:nite
  (:import-from #:nite.handler
                #:define-handler
                #:define-handler-set)
  (:import-from #:nite.router
                #:*router*
                #:router
                #:find-route
                #:find-route-uri
                #:connect
                #:mount
                #:rebuild-route-map)
  (:import-from #:nite.app
                #:define-app
                #:with-app
                #:app
                #:*request*
                #:*debug*)
  (:export
   #:*request*
   #:*router*
   #:define-handler
   #:define-handler-set
   #:define-app
   #:with-app
   #:app
   #:router
   #:find-route
   #:find-route-uri
   #:connect
   #:mount
   #:rebuild-route-map))
