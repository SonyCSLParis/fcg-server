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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains a handle-http-request method and additional code for handling calls ;;
;; to FCG from the server-interface package.                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; These default parameters will work locally on MAC OSX
;; To customize, you can set them in your init file...

(defparameter *server-interface-output-dir* (if (boundp 'cl-user::*server-interface-output-dir*)
                                              (eval 'cl-user::*server-interface-output-dir*)
                                              "~/Sites/FCG/")) ;; Make sure this folder exists (for permission reasons)!

(defparameter *server-interface-output-address* (if (boundp 'cl-user::*server-interface-output-address*)
                                              (eval 'cl-user::*server-interface-output-address*)
                                              "http://localhost/~paul/FCG/"))


(defparameter *static-html-busy* nil)

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-comprehend)))
  "Handle method for comprehending via the server-interface. Needs in the datafield:
   - handler-method=fcg-comprehend
   - utterance=...
   - cxn-inventory=...
   - package=...
   - visualisation=... (t,text or link-only)
   - monitor=... (monitor)"
  (let* (;; READ DATA
         (data (cdr (assoc :data http-request)))
         (utterance (cdr (assoc :utterance data)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data))))
         (package (upcase (cdr (assoc :package data))))
         (visualisation (upcase (cdr (assoc :visualisation data))))
         (monitor (upcase (cdr (assoc :monitor data))))
         ;; Preprocessing
         (utterance (cl-ppcre:regex-replace-all "%20" utterance " ")))
    ;; Some Checks
    (let ((error (check-input-http-request package cxn-inventory monitor visualisation)))
      (when error
        (return-from handle-http-request error)))
    ;; Set values
    (setf cxn-inventory (eval (find-symbol cxn-inventory package)))
    (setf monitor (eval (find-symbol monitor 'fcg)))
    ;; Return output
    (cond ((equalp "TEXT" visualisation)
           (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (monitors:with-disabled-monitor-notifications
                                                                                       (comprehend utterance :cxn-inventory cxn-inventory)))))))
          ((or (equalp "T" visualisation) (equalp "LINK-ONLY" visualisation))
           (loop while *static-html-busy* do (sleep 1))
           (let ((html-file
                  (with-static-html-busy 
                    (wi::create-static-html-page-server-interface
                        "FCG Online in Comprehension"
                      (monitors:with-activated-monitor monitor
                        (comprehend utterance :cxn-inventory cxn-inventory))))))
             (cond ((equalp "LINK-ONLY" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(string-append *server-interface-output-address* (last-elt (pathname-directory html-file)))))))
                   ((equalp "T" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (make-html-code-for-iframe-fcg-interactive html-file))))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-formulate)))
  "Handle method for comprehending via the server-interface. Needs in the datafield:
   - handler-method=fcg-comprehend
   - meaning=...
   - cxn-inventory=...
   - package=...
   - visualisation=... (t,text or link-only)
   - monitor=... (monitor)"
  (let* (;; READ DATA
         (data (cdr (assoc :data http-request)))
         (package (upcase (cdr (assoc :package data))))
         (meaning (with-package package
                    (read-from-string
                     (if (equalp "((" (subseq (cdr (assoc :meaning data)) 0 2))
                       (cl-ppcre:regex-replace-all "%20" (cdr (assoc :meaning data)) " ")
                       (string-append "(" (cl-ppcre:regex-replace-all "%20" (cdr (assoc :meaning data)) " ") ")")))))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data))))
         (visualisation (upcase (cdr (assoc :visualisation data))))
         (monitor (upcase (cdr (assoc :monitor data)))))
    ;; Some Checks
    (let ((error (check-input-http-request package cxn-inventory monitor visualisation)))
      (when error
        (return-from handle-http-request error)))
    ;; Set values
    (setf cxn-inventory (eval (find-symbol cxn-inventory package)))
    (setf monitor (eval (find-symbol monitor 'fcg)))
    ;; Return output
    (cond ((equalp "TEXT" visualisation)
           (cl-json:encode-json-alist-to-string `((:fcg-response . ,(monitors:with-disabled-monitor-notifications (format nil "~{~a~^ ~}" (formulate meaning :cxn-inventory cxn-inventory)))))))
          ((or (equalp "T" visualisation) (equalp "LINK-ONLY" visualisation))
           (loop while *static-html-busy* do (sleep 1))
           (let ((html-file
                  (with-static-html-busy
                    (wi::create-static-html-page-server-interface  "FCG Online in Comprehension"
                      (monitors:with-activated-monitor monitor (formulate meaning :cxn-inventory cxn-inventory))))))
             (cond ((equalp "LINK-ONLY" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(string-append *server-interface-output-address* (last-elt (pathname-directory html-file)))))))
                   ((equalp "T" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (make-html-code-for-iframe-fcg-interactive html-file))))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-comprehend-and-formulate)))
  "Handle method for comprehending and formulating via the server-interface. Needs in the datafield:
   - handler-method=fcg-comprehend-and-formulate
   - utterance=...
   - cxn-inventory=...
   - package=...
   - visualisation=... (t,text or link-only)
   - monitor=... (monitor)"
  (let* (;; READ DATA
         (data (cdr (assoc :data http-request)))
         (utterance (cdr (assoc :utterance data)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data))))
         (package (upcase (cdr (assoc :package data))))
         (visualisation (upcase (cdr (assoc :visualisation data))))
         (monitor (upcase (cdr (assoc :monitor data))))
         ;; Preprocessing
         (utterance (cl-ppcre:regex-replace-all "%20" utterance " ")))
    ;; Some Checks
    (let ((error (check-input-http-request package cxn-inventory monitor visualisation)))
      (when error
        (return-from handle-http-request error)))
    ;; Set values
    (setf cxn-inventory (eval (find-symbol cxn-inventory package)))
    (setf monitor (eval (find-symbol monitor 'fcg)))
    ;; Return output
    (cond ((equalp "TEXT" visualisation)
           (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~{~a~^ ~}" (monitors:with-disabled-monitor-notifications
                                                                                              (comprehend-and-formulate utterance :cxn-inventory cxn-inventory)))))))
          ((or (equalp "T" visualisation) (equalp "LINK-ONLY" visualisation))
           (loop while *static-html-busy* do (sleep 1)) ;; Bottleneck, but I don't see another solution
           (let ((html-file
                  (with-static-html-busy
                    (wi::create-static-html-page-server-interface
                     "FCG Online in Comprehension"
                      (monitors:with-activated-monitor monitor (fcg::comprehend-and-formulate utterance :cxn-inventory cxn-inventory))))))
             (cond ((equalp "LINK-ONLY" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(string-append *server-interface-output-address* (last-elt (pathname-directory html-file)))))))
                   ((equalp "T" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (make-html-code-for-iframe-fcg-interactive html-file))))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-formulate-and-comprehend)))
  "Handle method for comprehending via the server-interface. Needs in the datafield:
   - handler-method=fcg-comprehend
   - meaning=...
   - cxn-inventory=...
   - package=...
   - visualisation=... (t,text or link-only)
   - monitor=... (monitor)"
  (let* (;; READ DATA
         (data (cdr (assoc :data http-request)))
         (package (upcase (cdr (assoc :package data))))
         (meaning (with-package package
                    (read-from-string
                     (if (equalp "((" (subseq (cdr (assoc :meaning data)) 0 2))
                       (cl-ppcre:regex-replace-all "%20" (cdr (assoc :meaning data)) " ")
                       (string-append "(" (cl-ppcre:regex-replace-all "%20" (cdr (assoc :meaning data)) " ") ")")))))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data))))
         (visualisation (upcase (cdr (assoc :visualisation data))))
         (monitor (upcase (cdr (assoc :monitor data)))))
    ;; Some Checks
    (let ((error (check-input-http-request package cxn-inventory monitor visualisation)))
      (when error
        (return-from handle-http-request error)))    
    ;; Set values
    (setf cxn-inventory (eval (find-symbol cxn-inventory package)))
    (setf monitor (eval (find-symbol monitor 'fcg)))
    ;; Return output
    (cond ((equalp "TEXT" visualisation)
           (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (monitors:with-disabled-monitor-notifications (format nil "~{~a~^ ~}"
                                                             (fcg::formulate-and-comprehend meaning :cxn-inventory cxn-inventory))))))))
          
          ((or (equalp "T" visualisation) (equalp "LINK-ONLY" visualisation))
           (loop while *static-html-busy* do (sleep 1)) ;; Bottleneck, but I don't see another solution
           (let ((html-file
                  (with-static-html-busy
                      (wi::create-static-html-page-server-interface
                       "FCG Online in Comprehension"
                        (monitors:with-activated-monitor monitor (fcg::formulate-and-comprehend meaning :cxn-inventory cxn-inventory))))))
             (cond ((equalp "LINK-ONLY" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(string-append *server-interface-output-address* (last-elt (pathname-directory html-file)))))))
                   ((equalp "T" visualisation)
                    (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~a" (make-html-code-for-iframe-fcg-interactive html-file))))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-get-cxn-inventory)))
  "Handle method returning a link to the html page of a cxn-inventory."
  (let* (;; READ DATA
         (data (cdr (assoc :data http-request)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data)))))
    ;; Set values
    (let ((link-to-cxn-inv (cdr (assoc cxn-inventory *links-cxn-inv* :test 'equalp))))
        (cl-json:encode-json-alist-to-string `((:fcg-response . ,(string-append *server-interface-output-address*  link-to-cxn-inv)))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-get-example-sentences)))
  "Handle method for returning example sentences"
  (let* ((data (cdr (assoc :data http-request)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data)))))
    (let ((example-sentences (cdr (assoc cxn-inventory *example-sentences* :test 'equalp))))
      (when example-sentences
        (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~{~a~^,~}" (random-elts example-sentences  (min 15 (length example-sentences)))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-get-example-meanings)))
  "Handle method for returning example meanings"
  (let* ((data (cdr (assoc :data http-request)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data)))))
    (let ((example-meanings (cdr (assoc cxn-inventory *example-meanings* :test 'equalp))))
      (when example-meanings
        (cl-json:encode-json-alist-to-string `((:fcg-response . ,(format nil "~{~a~^,~}" (random-elts example-meanings (min 15 (length example-meanings)))))))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-get-reference-text)))
  "Handle method for returning reference text of grammar (with name and publications)"
  (let* ((data (cdr (assoc :data http-request)))
         (cxn-inventory (upcase (cdr (assoc :cxn-inventory data)))))
    (let ((reference-text (cdr (assoc cxn-inventory *reference-texts* :test 'equalp))))
      (when reference-text
        (cl-json:encode-json-alist-to-string `((:fcg-response . ,reference-text)))))))

(defmethod handle-http-request (http-request request-type (handler-method (eql :fcg-static-html-busy)))
  "handler method for checking whether *static-html-busy* is true"
  (cl-json:encode-json-alist-to-string `((:fcg-response . ,*static-html-busy*))))
