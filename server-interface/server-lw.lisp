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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up an Babel2 Server for LISPWORKS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+:lispworks 
(defun start-server (&key (port *port*))
  "Starts a Babel2 server listening to a port."
  (let ((process-name (format nil "Babel2 Server (port ~D)" port)))
    (if (mp:get-process process-name)
      (warn (format nil "Port ~D is already in use. Could not establish connection." port))
      (comm:start-up-server :function 'make-io-stream
                            :service port
                            :process-name process-name))))

#+:lispworks 
(defun stop-server (&key (port *port*))
  "Stops a Babel2 server process."
  (let ((process (mp:get-process (format nil "Babel2 Server (port ~D)" port))))
    (if process
      (progn
        (mp:process-kill process)
        (format nil "Babel2 Server (port ~D) closed" port))
      (format nil "Babel2 Server (port ~D) could not be found" port))))

#+:lispworks 
(defun make-io-stream (handle)
  "makes an IO stream and invokes talk-on-stream function"
  (let ((stream (make-instance 'comm:socket-stream
                               :socket handle
                               :direction :io
                               :element-type 'base-char)))
    (mp:process-run-function (format nil "Babel2 Server Process ~D" handle)
                             '()
                             'talk-on-stream stream)))
