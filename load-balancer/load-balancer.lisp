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

(in-package :server-interface)

(defparameter *load-balancer-port* (if (boundp 'cl-user::*load-balancer-port*)
                               (eval 'cl-user::*load-balancer-port*)
                               8920))

(defun balance-load (stream)
  "reads input on stream, parses the http request, distributes its to the
   right handler-method and writes back the output to the stream."
  (when (listen stream)
    (let* ((request (parse-http-request stream))
           (data (cdr (assoc :data request)))
           (idle-port (find-idle-port))
           (host (format nil "http://~a:~a" *host* idle-port))
           (response (remove-trailing-cariage-return
                            (first (exec-and-return "curl" "-s" host "-d"
                                                    (cl-json-shell::encode-json-to-string-for-shell data)))))) 
      (report-http-request idle-port request)
      (http-send-line stream "HTTP/1.1 200 OK Content-Type: application/json")
      (http-send-line stream (format nil "Allow: POST"))
      (http-send-line stream (format nil "Accept: application/json"))
      (http-send-line stream (format nil "Content-Type: application/json"))
      (http-send-line stream (format nil "Access-Control-Allow-Headers: Content-Type"))
      (http-send-line stream (format nil "Access-Control-Allow-Origin:~a" "*"))
      (http-send-line stream "")
      (format stream "~a" response)
      (http-send-line stream ""))
    (force-output stream)
    (close stream)))

(defun find-idle-port ()
  "returns a port on which static html is not busy"
  (let (idle-port)
    (loop while (not idle-port)
          do
          (let* ((port (random-elt fcg-server::*server-ports*))
                 (host (format nil "http://~a:~a" *host* port))
                 (busy (cdr (assoc :fcg-response
                                   (cl-json:decode-json-from-string
                           (remove-trailing-cariage-return
                            (first (exec-and-return "curl" "-s" host "-d"
                                                    (cl-json-shell::encode-json-to-string-for-shell '(("handler-method" . "fcg-static-html-busy")))))))))))
                 (unless busy
                   (setf idle-port port))))
          idle-port))
  
(defun report-http-request (port request)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (format t "~%~d/~2,'0d/~2,'0d at ~2,'0d:~2,'0d:~2,'0d~%" day month year hour min sec))
  (format t "New incoming call:~%")
  (format t "    Dispatched to port: ~a~%" port)
  (format t "    Request: ~{~a~^~%    ~}~%" request))