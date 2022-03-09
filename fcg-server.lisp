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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; This file contains all code concerning the set-up of an FCG server ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :hunchentoot)

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.

(export '(fcg-acceptor))

(defclass fcg-acceptor (easy-acceptor)
  ())

(defmethod acceptor-dispatch-request ((acceptor fcg-acceptor) request)
  (loop for dispatcher in *dispatch-table*
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

(defmethod acceptor-dispatch-request :around ((acceptor fcg-acceptor) request)
  (declare (ignore request))
  (setf (header-out "Access-Control-Allow-Origin") "*")
  (setf (header-out "Access-Control-Allow-Headers") "Content-Type,Accept,Origin")
  (setf (header-out "Content-Type") "application/json")
  (call-next-method))

(in-package :fcg-server)

(defvar *fcg-server* nil
  "Global variable that holds the FCG server")

(defun start-fcg-server (&key (address "127.0.0.1")
                              (port 1170)
                              (grammar-systems '(:fcg)))
  "Starting up the fcg-server..."
  (ql:quickload grammar-systems)
  (if *fcg-server*
    (warn (format nil "Server already running at ~a:~a" (hunchentoot:acceptor-address *fcg-server*) (hunchentoot:acceptor-port *fcg-server*)))
    (progn
      (push (snooze:make-hunchentoot-app) hunchentoot:*dispatch-table*)
      (setf *fcg-server* (hunchentoot:start (make-instance 'hunchentoot::fcg-acceptor :address address :port port)))
      (format t "***** started FCG server at ~a:~a *****" (hunchentoot:acceptor-address *fcg-server*) (hunchentoot:acceptor-port *fcg-server*))
      *fcg-server*)))

;; (start-fcg-server)

(defun stop-fcg-server ()
  "Stops the FCG server."
  (when *fcg-server*
    (hunchentoot:stop *fcg-server*)
    (setf *fcg-server* nil)))

;; (stop-fcg-server)


(defmethod explain-condition ((condition http-condition)
                              resource
                              ct)
  "Send back detailed error reports."
  (encode-json-to-string
   `((:status-code . ,(format nil "~a" (status-code condition)))
     (:error-message . ,(simple-condition-format-control condition)))))

