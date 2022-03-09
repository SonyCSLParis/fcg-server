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

(asdf:operate 'asdf:load-op :server-interface)

(in-package :server-interface)

#+:lispworks
(block port-loop
  (loop for port in '(8921 8922 8923 8924)
        do
        (let ((exit-status (run-prog (format nil "curl 'http://localhost:~a'" port))))
          (when (= exit-status 7)
            (start-server :port port)
            (return-from port-loop (format nil "server started at port ~a" port))))))

  
#-:lispworks
(block port-loop
    (loop for port in '(8921 8922 8923 8924)
          do
          (let* ((stream nil)
                 (port-free-p nil)
                 (host (format nil "http://~a:~a" si::*host* port)))
            (setf stream (pipe-input "curl" :args `("-s" ,host) :wait t))
            (setf port-free-p (not (listen stream)))
            (close stream)
            (when port-free-p
              (start-server :port port)
              (return-from port-loop (format nil "server started at port ~a" port))))))

(asdf:operate 'asdf:load-op :fcg-server)
