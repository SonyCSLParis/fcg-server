;; Copyright 2022-present Sony Computer Science Laboratories Paris

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                     ;;
;; FCG server demonstration                            ;;
;;                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.


;; Loading ... ;;
;;;;;;;;;;;;;;;;;

(ql:quickload :fcg-server)
(in-package :fcg-server)



;; About the FCG server        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The FCG server is a package that sets up a REST architecture for interacting with FCG grammars.


;; Launching the FCG server        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The FCG server can be launched by evaluating the following s-expression:

(start-fcg-server :address "127.0.0.1"
                  :port 1170
                  :grammar-systems '(:fcg)) 

;; Address and port indicate the address and port at which the server is running.
;; Grammar systems quickloads the systems that in turn load the grammars that the server knows about.


;; Testing the FCG server        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's load a grammar
(fcg:load-demo-grammar)


;; By default, the server listens to two main routes: comprehend-utterance and produce-utterance.
;; These routes accept post requests with data encoded as json.
;; Grammar, package and maximum timeout need to be provided.

;; To try out the different routes in the terminal, you can use curl in a terminal window.
;; For example:

;; curl 127.0.0.1:1170/comprehend-utterance -H "Content-Type: application/json" -d '{"utterance":"the linguist likes the mouse", "package": "fcg", "grammar": "*fcg-constructions*", "timeout": 3}'

;; curl localhost:1170/produce-utterance -H "Content-Type: application/json" -d '{"meaning": [["mouse", "x"], ["unique", "x"], ["linguist", "y"], ["unique", "y"], ["deep-affection", "y", "x"]], "package": "fcg", "grammar": "*fcg-constructions*", "timeout": 1}'

;; Stopping the FCG server        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stopping the FCG server can be done with the following function call:
(stop-fcg-server)





