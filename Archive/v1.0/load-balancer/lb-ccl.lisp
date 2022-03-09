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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up an Babel2 Server for CCL    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:ccl
(defvar *server* nil) ;; will hold the server process

#+:ccl
(defun start-load-balancer (&key (port *load-balancer-port*))
  "Starts a Babel2 server listening to a port."
  (if *server*
      (warn (format nil "Port ~D is already in use. Could not establish connection." port))
      (let ((socket (usocket:socket-listen *host*
                                           port
                                           :reuse-address t)))
        (setf *server* (try-make-thread
                            (format nil "Babel2 Load Balancer (port ~D)" port)
                            (lambda () (unwind-protect
                                            (run-load-balancer socket)
                                         (usocket:socket-close socket))))))))

#+(and bordeaux-threads ccl)
(defun stop-load-balancer (&key (port *load-balancer*))
  "Stops a Babel2 server process."
  (if *server*
      (progn
        (let ((server (shiftf *server* nil))) 
          (when server
            (bt:destroy-thread server)))
        (format nil "Babel2 Load Balancer (port ~D) closed" port))
      (format nil "Babel2 Load Balancer (port ~D) could not be found" port)))

#+:ccl
(defun run-load-balancer (socket)
  "Loop around, waiting for incoming connections. Each time one arrives, 
   call usocket:socket-stream to create a bidirectional stream and pass
   this to handle-request, asynchronously if possible. Guarantee that the 
   stream will be closed when handle-request exits."
  (loop
   (usocket:wait-for-input socket)
   (let ((stream (usocket:socket-stream (usocket:socket-accept socket))))
     (try-make-thread (format nil "Babel2 Load Balancer Process ~D" stream) 
                      (lambda ()
                        (with-open-stream (stream stream) (balance-load stream)))))))

