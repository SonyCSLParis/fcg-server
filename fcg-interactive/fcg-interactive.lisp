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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; This file contains all code concerning the routes used by FCG interactive  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :fcg-server)

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Comprehending utterances ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute comprehend-utterance-fcg-interactive (:options :text/*))

(defroute comprehend-utterance-fcg-interactive (:post :application/json)
  (fcg-server-comprehend-utterance-fcg-interactive
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error ()
       (http-condition 400 "Malformed JSON!")))))


(defun fcg-server-comprehend-utterance-fcg-interactive (json-input)
  "Handles the comprehend-utterance-fcg-interactive route."
  
  ;; 1. Assert that all required keys are present
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (check-for-missing-keys json-input '(:utterance :package :grammar :timeout))

  ;; 2. Retrieve utterance, package and grammar
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (let* (;; Retrieve the utterance and assert that it is a string
         (utterance-raw (cdr (assoc :utterance json-input)))
         (utterance (if (stringp utterance-raw)
                      utterance-raw
                      (http-condition 400 (format nil "Utterance should be of type string. Received a ~a." (type-of utterance-raw)))))
         ;; Retrieve the package and assert that is found
         (package-raw (cdr (assoc :package json-input)))
         (package (if (find-package (utils:make-kw package-raw))
                    (find-package (utils:make-kw package-raw))
                    (http-condition 400 (format nil "Package '~a' not found." package-raw))))
         ;; Retrieve the grammar and assert that it is found
         (grammar-raw (cdr (assoc :grammar json-input)))
         (grammar (if (find-symbol (utils:upcase grammar-raw) package)
                    (symbol-value (find-symbol (utils:upcase grammar-raw) package))
                    (http-condition 400 (format nil "Grammar '~a' not found in package '~a'." grammar-raw package-raw ))))
                   ;; Retrieve the timeout and check if it is a number
         (timeout-raw (cdr (assoc :timeout json-input)))
         (timeout (if (numberp timeout-raw)
                    timeout-raw
                    (http-condition 400 (format nil "Timeout should be of type number. Received '~a'." timeout-raw )))))

    ;; 3. Perform the actual comprehension process
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (handler-case
        (trivial-timeout:with-timeout (timeout)
          (multiple-value-bind (meaning cipn)
              (handler-case (monitors:with-disabled-monitor-notifications
                              (fcg:comprehend utterance  :cxn-inventory grammar))
                (error (e)
                  (http-condition 400 (format nil "Error during the comprehension process: ~a" e))))
      
            ;; 4. Get constructional dependencies from solution cip-node as svg
            (let ((constructional-dependencies (fcg::get-constructional-dependencies cipn :format "svg" :open nil)))
        
              ;;  5. Return the result as a json object
              (encode-json-alist-to-string `((:status-code . 200)
                                             (:meaning . ,(utils:list-of-predicates->string-of-predicates meaning))
                                             (:constructional-dependencies . ,constructional-dependencies))))))
      (trivial-timeout:timeout-error ()
        (snooze:http-condition 500 "Timeout exceeded!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;; Producing utterances     ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defroute produce-utterance-fcg-interactive (:options :text/*))

(defroute produce-utterance-fcg-interactive (:post :application/json)
  (fcg-server-produce-utterance-fcg-interactive
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error (e)
       (http-condition 400 (format nil "Malformed JSON error: ~a." e))))))



(defun fcg-server-produce-utterance-fcg-interactive (json-input)
  "Handles the produce-utterance-fcg-interactive route."
  
  ;; 1. Assert that all required keys are present
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (check-for-missing-keys json-input '(:meaning :package :grammar :timeout))

  ;; 2. Retrieve utterance, package and grammar
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (let* (;; Retrieve the meaning, assert that it is a string and turn it into a list of predicates
         (meaning-raw (cdr (assoc :meaning json-input)))
         (meaning (if (stringp meaning-raw)
                    (utils:string-of-predicates->list-of-predicates meaning-raw)
                    (http-condition 400 (format nil "Meaning should be of type string (e.g. 'mouse(x), unique(x)'. Received a ~a." (type-of meaning-raw)))))
         ;; Retrieve the package and assert that is found
         (package-raw (cdr (assoc :package json-input)))
         (package (if (find-package (utils:make-kw package-raw))
                    (find-package (utils:make-kw package-raw))
                    (http-condition 400 (format nil "Package '~a' not found." package-raw))))
         ;; Retrieve the grammar and assert that it is found
         (grammar-raw (cdr (assoc :grammar json-input)))
         (grammar (if (find-symbol (utils:upcase grammar-raw) package)
                    (symbol-value (find-symbol (utils:upcase grammar-raw) package))
                    (http-condition 400 (format nil "Grammar '~a' not found in package '~a'." grammar-raw package-raw ))))
         ;; Retrieve the timeout and check if it is a number
         (timeout-raw (cdr (assoc :timeout json-input)))
         (timeout (if (numberp timeout-raw)
                    timeout-raw
                    (http-condition 400 (format nil "Timeout should be of type number. Received '~a'." (type-of timeout-raw))))))

    ;; 3. Perform the actual production process
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (handler-case
        (trivial-timeout:with-timeout (timeout)
          (multiple-value-bind (utterance cipn)
              (handler-case (monitors:with-disabled-monitor-notifications
                              (fcg:formulate meaning  :cxn-inventory grammar))
                (error (e)
                  (http-condition 400 (format nil "Error during the comprehension process: ~a" e))))

            ;; 4. Get constructional dependencies from solution cip-node as svg
            (let ((constructional-dependencies (fcg::get-constructional-dependencies cipn :format "svg" :open nil)))
       
              ;;  5. Return the result as a json object
              (encode-json-alist-to-string `((:status-code . 200)
                                             (:utterance . ,(if (listp utterance)
                                                              (format nil "~{~a~^ ~}" utterance)
                                                              utterance))
                                             (:constructional-dependencies . ,constructional-dependencies))))))
      (trivial-timeout:timeout-error ()
        (snooze:http-condition 500 "Timeout exceeded!")))))
                    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              ;;
;; Example utterances/meanings  ;;
;;                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defroute examples-fcg-interactive (:options :text/*))

(defroute examples-fcg-interactive (:post :application/json)
  (fcg-server-examples
   (handler-case
       (decode-json-from-string
        (payload-as-string))
     (error ()
       (http-condition 400 "Malformed JSON!")))))

(defun fcg-server-examples (json-input)
  "Handles the example-utterances route."
  
  ;; 1. Assert that all required keys are present
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (check-for-missing-keys json-input '(:direction :package :grammar))

  ;; 2. Retrieve direction, package and grammar
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (let* (;; Retrieve the direction and assert that it is a string
         (direction-raw (cdr (assoc :direction json-input)))
         (direction (if (or (string= direction-raw "comprehension")
                            (string= direction-raw "production"))
                      direction-raw
                      (http-condition 400 (format nil "Direction should be either 'comprehension' or 'production'. Received ~a." direction-raw))))
         ;; Retrieve the package and assert that is found
         (package-raw (cdr (assoc :package json-input)))
         (package (if (find-package (utils:make-kw package-raw))
                    (find-package (utils:make-kw package-raw))
                    (http-condition 400 (format nil "Package '~a' not found." package-raw))))
         ;; Retrieve the grammar and assert that it is found
         (grammar-raw (cdr (assoc :grammar json-input)))
         (grammar (if (find-symbol (utils:upcase grammar-raw) package)
                    (find-symbol (utils:upcase grammar-raw) package)
                    (http-condition 400 (format nil "Grammar '~a' not found in package '~a'." grammar-raw package-raw)))))

    ;; 3. Perform the actual lookup
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (handler-case (let ((examples (if (string= direction "comprehension")
                                    (cdr (assoc grammar *example-utterances*))
                                    (cdr (assoc grammar *example-meanings*)))))

                    (encode-json-alist-to-string `((:status-code . 200)
                                                   (:examples . ,examples))))
                   
      (error (e)
        (http-condition 400 (format nil "Error fetching examples: ~a" e))))))


