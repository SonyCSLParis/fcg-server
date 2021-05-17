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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load system for the grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :english-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to cxn-inventory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here, you should add a pointer to a folder containing static html for your cxn-inventory
;; You can make it with the function make-html-cxn-inventory-for-fcg-interactive , e.g.
;; (fcg::make-html-cxn-inventory-for-fcg-interactive fcg::*fcg-constructions* :hide-labels '(fcg::lex fcg::lex2 fcg::marked-morph fcg::zero-morph))
;; Then put it somewhere /var/www/wordpress/FCG/cxn-inventories - or talk to paul

(pushnew '("*fcg-constructions*" . "cxn-inventories/fcg-constructions/") *links-cxn-inv*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to examples     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you should add links to text files containing example sentences/meanings

(push (cons "*fcg-constructions*" (fcg-server::read-example-sentences
                                              (babel-pathname  :directory '("grammars" "English" "example-sentences") :name "sentences" :type "txt")))
         fcg-server::*example-sentences*)

(push (cons "*fcg-constructions*" (fcg-server::read-example-meanings
                                              (babel-pathname  :directory '("grammars" "English" "example-sentences") :name "meanings" :type "txt")))
         fcg-server::*example-meanings*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to reference text  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((reference-text
       (string-append (render-xml '((div :class "ref_web_demo")((p)"English Grammar by Remi van Trijp")))
                      (render-xml '((div :class "ref_supporting_publications")
                                    ((p) "Supporting publications:")
                                    ((ul)
                                     ((li)"van Trijp, R. 2017. A Computational Construction Grammar for English. The AAAI 2017 Spring Symposium on Computational Construction Grammar and Natural Language Understanding. Technical Report SS-17-02.")))))))
      (push `("*fcg-constructions*" . ,reference-text)  *reference-texts*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar-specific code     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you can add any code that you want to execute on the startup of your grammar.
;; (you might want to switch to your a different package here)



