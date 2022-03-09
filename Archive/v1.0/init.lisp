;;
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

(in-package :fcg-server)

;; ###
;; Initialisation file that is loaded when the :fcg-server system is loaded.
;; ###

;; Ports on which the server is running
(defparameter *server-ports* (loop for n from 8921 upto 8924 collect n))

;; For links to cxn-inv
(defparameter *links-cxn-inv* '())

;; For links to example-sentences
(defparameter *example-sentences* '())
(defparameter *example-meanings* '())

;; For links to reference text
(defparameter *reference-texts* '())

;; Load init files for different grammars
;;(load (babel-pathname :directory '("sharing" "fcg-server") :name "init-test" :type "lisp"))
;;(load (babel-pathname :directory '("sharing" "fcg-server") :name "init-portuguese" :type "lisp"))
;; (load (babel-pathname :directory '("sharing" "fcg-server") :name "init-english" :type "lisp"))
;;(load (babel-pathname :directory '("sharing" "fcg-server") :name "init-russian" :type "lisp"))
