;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

;;; A web interface to the rule sets of an agent

(in-package :asdf)

(defsystem :server-interface
  :description "A server interface to Lisp."
  :depends-on (:utils :bordeaux-threads :cl-ppcre :usocket :cl-json)
  :components 
  ((:file "package")
   (:file "http-request")
   (:file "server-interface")
   (:file "server-lw")
   (:file "server-ccl")))



