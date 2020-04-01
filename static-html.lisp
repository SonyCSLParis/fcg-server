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

(in-package :web-interface)

(defmacro create-static-html-page-server-interface (title &body body)
  "Wraps some code that sends output to the web interface to create a
   static page that can be viewed without a lisp running in the
   background."
  `(let* ((*static-html-output-dir*
           (merge-pathnames  
            (make-pathname  :directory
                            (cons :relative (list (multiple-value-bind (sec min hour day month year)
                                                      (decode-universal-time (get-universal-time))
                                                    (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-html" 
                                                            year month day hour min sec)))))
            fcg-server::*server-interface-output-dir*))
          (html-file (merge-pathnames 
                      *static-html-output-dir*
                      (make-pathname :name "index" :type "html")))
          (js-file (make-pathname :directory (pathname-directory
                                              *static-html-output-dir*)
                                  :name "AsyncXMLHttpRequest" :type "js")))
     (setf *static-elements* nil)
     (clear-page)
     (ensure-directories-exist *static-html-output-dir*)
     (utils:run-prog  "cp" :args (list (mkstr *AsyncXMLHttpRequest.js*)
                                       (mkstr js-file)))
     (let ((*static-html* t))
       ,@body)
     (with-open-file (stream html-file :direction :output :if-exists :supersede)
       (write-static-page stream ,title)
       (force-output stream))
     html-file))
