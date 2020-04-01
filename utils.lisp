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

(defun read-example-sentences (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun read-example-meanings (path)
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
          while line
       collect (read-from-string line))))

(defmacro with-static-html-busy (&body body)
  `(progn
     (setf *static-html-busy* t)
     (unwind-protect (progn ,@body)
       (setf *static-html-busy* nil))))

(defun check-input-http-request (package cxn-inventory monitor visualisation)
  "checks some inputs from web-interface to method-handlers"
  (cond ((not (find-package package))
         (return-from check-input-http-request
           (format nil "Specified package ~D not found." package)))
        ((not (find-symbol cxn-inventory (find-package package)))
         (return-from check-input-http-request
           (format nil "Cxn-inventory ~D in package ~D not found." cxn-inventory package)))
        ((not (or (eq monitor nil)
                  (eql 'monitors::monitor (type-of (eval (find-symbol monitor 'fcg))))))
         (return-from check-input-http-request
           (format nil "Monitor ~D is not of type MONITOR, but of type ~D." monitor (type-of (eval (find-symbol monitor 'fcg))))))
        ((<= (length (constructions (eval (find-symbol cxn-inventory (find-package package))))) 0)
         (return-from check-input-http-request
           (format nil "Cxn-inventory ~D in package ~D, of type ~D is EMPTY."
                   cxn-inventory package (type-of (eval (find-symbol cxn-inventory package))))))
        ((not (or (equalp visualisation "T")
                  (equalp visualisation "LINK-ONLY")
                  (equalp visualisation "TEXT")))
         (return-from check-input-http-request
           (format nil "Visualisation option ~D not valid (choose TEXT, T or LINK-ONLY)"  visualisation)))))

(defun make-html-code-for-iframe-fcg-interactive (html-file)
  (string-append
   (render-xml `((html)
                 ((head)
                  ((title) "FCG Online"))
                 ((body)
                  ((iframe :onload "resizeIframe(this)"
                           :style "border:0;"
                           :width "100%"
                           :height "100%"
                           :src ,(string-append *server-interface-output-address*
                                                (last-elt (pathname-directory html-file))))))))
   (render-xml '((script :language "javascript"
                         :type "text/javascript")
                 "function resizeIframe(obj) {obj.style.height = obj.contentWindow.document.body.scrollHeight + 'px';}"))))

(in-package :fcg)

(defun make-html-cxn-inventory-for-fcg-interactive (cxn-inventory &key (hide-labels '(lex morph)))
  (if hide-labels
    ;; make html of subset of inventory
    (let ((sub-inventory (copy-object cxn-inventory)))
      (dolist (cxn (constructions sub-inventory))
        (let* ((cxn-label (get-configuration (attributes cxn) :label))
               (label-list-p (listp cxn-label)))
          (cond ((and (not label-list-p) (member cxn-label hide-labels))
                 (with-disabled-monitor-notifications (delete-cxn cxn sub-inventory))
                 (format t "x"))
                ((and label-list-p (< (length (set-difference cxn-label hide-labels)) (length cxn-label)))
                 (with-disabled-monitor-notifications (delete-cxn cxn sub-inventory))
                 (format t "x"))
                (t (format t ".")))))
      (format t "Creating Static Html for ~a constructions." (size sub-inventory))
      (create-static-html-page (symbol-name (name cxn-inventory)) 
        (add-element (make-html sub-inventory :expand-initially t))))
    ;; Otherwise make html of whole inventory
    (create-static-html-page (symbol-name (name cxn-inventory))
      (add-element (make-html cxn-inventory :expand-initially t)))))



