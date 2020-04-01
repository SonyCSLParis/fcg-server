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

(in-package :fcg)

;; ############################################################
;; trace-fcg-interactive
;; ############################################################

(define-monitor trace-fcg-interactive 
    :documentation "Traces results of fcg processing in an interactive web browser")

;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg-interactive cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; ------------------------------------------------------------
;; construction set application


(define-event-handler (trace-fcg-interactive cip-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution)))))

(define-event-handler (trace-fcg-interactive produce-started)
  (add-element `((hr)))
  (add-element `((h2) "Formulating&#160;"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event-handler (trace-fcg-interactive produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))
     
(define-event-handler (trace-fcg-interactive parse-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Comprehending &quot;~{~a~^ ~}&quot;" utterance))))

(define-event-handler (trace-fcg-interactive parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning)
                 (html-pprint meaning)))
  (add-element `((p) " ")))



;; ############################################################
;; trace-fcg-interactive-no-car
;; ############################################################

(define-monitor trace-fcg-interactive-no-car 
    :documentation "Traces results of fcg processing, no construction-application-result")

;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg-interactive-no-car cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; ------------------------------------------------------------
;; construction set application


(define-event-handler (trace-fcg-interactive-no-car cip-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-interactive-no-car cip :solutions (when solution (list solution))))
  )

(define-event-handler (trace-fcg-interactive-no-car produce-started)
  (add-element `((hr)))
  (add-element `((h2) "Formulating&#160;"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event-handler (trace-fcg-interactive-no-car produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))
     
(define-event-handler (trace-fcg-interactive-no-car parse-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Comprehending &quot;~{~a~^ ~}&quot;" utterance))))

(define-event-handler (trace-fcg-interactive-no-car parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning)
                 (html-pprint meaning))) 
  (add-element `((p) " ")))


(defmethod make-html-fcg-interactive-no-car ((cip construction-inventory-processor) 
                                             &key
                                             (feature-types (feature-types (original-cxn-set (construction-inventory cip))))
                                             (solutions nil solutions-provided-p)
                                             (show-queue nil)
                                             (configuration nil))
  (declare (ignore solutions-provided-p show-queue))
  (let ((*make-html-cfs-construction-inventory* (construction-inventory cip))
        (configuration (or configuration (visualization-configuration (construction-inventory cip)))))
    (reset-expanded-node)
    (if (get-configuration configuration :expand-nodes-in-search-tree)
      `((div)
        ((table :class "two-col")
         ((tbody)
          ((tr)
           ((td) "initial structure")
           ((td) ,(make-html-fcg-light (initial-cfs cip)
                                       :configuration configuration :feature-types feature-types)))
          ,@(loop for solution in solutions 
                  for n from 1
                  append 
                  `(,(if (> (length solutions) 1)
                       `((tr) ((td :colspan "2") ((b) "solution " ,n)))
                       "")
                    ((tr)
                     ((td) "applied constructions")
                     ((td) ,@(html-hide-rest-of-long-list 
                              (get-applied-fcg-light-constructions (applied-constructions solution)) 10
                              #'(lambda (construction) 
                                  (make-html construction :expand-initially nil
                                             :configuration configuration
                                             :wrap-in-paragraph nil)))))
                    ((tr)
                     ((td) "resulting structure")
                     ((td) ,(make-html-fcg-light (car-resulting-cfs (cipn-car solution))
                                                 :configuration configuration :feature-types feature-types)))))))))))

