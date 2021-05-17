(in-package :fcg-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load system for the grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op 'fcg)
(load (babel-pathname :directory '("grammars" "Russian") :name "motion-verbs" :type "lisp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to cxn-inventory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here, you should add a pointer to a folder containing static html for your cxn-inventory
;; You can make it with the function make-html-cxn-inventory-for-fcg-interactive , e.g.
;; (fcg::make-html-cxn-inventory-for-fcg-interactive fcg::*portuguese-corpus-grammar* :hide-labels '(fcg::morph fcg::lex))
;; Then put it somewhere /var/www/wordpress/FCG/cxn-inventories - or talk to paul

(pushnew '("*russian-motion-grammar*" . "cxn-inventories/russian-motion-grammar/") *links-cxn-inv*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to examples     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you should add links to text files containing example sentences/meanings

(push (cons "*russian-motion-grammar*" (read-example-sentences
                                        (babel-pathname  :directory '("grammars" "Russian")
                                                         :name "example-sentences" :type "txt")))
      *example-sentences*)

(push (cons "*russian-motion-grammar*" (read-example-meanings
                                        (babel-pathname  :directory '("grammars" "Russian")
                                                         :name "example-meanings" :type "txt")))
      *example-meanings*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to reference text  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((reference-text (string-append
                       (render-xml '((div :class "ref_web_demo")
                                     ((p)"For a documented web demonstration supporting this grammar, click "
                                      ((a :href "https://www.fcg-net.org/demos/russian-motion-verbs") "here."))))
                       (render-xml '((div :class "ref_supporting_publications")
                                     ((p) "Supporting publication:")
                                     ((ul)
                                      ((li)"Knight, Yana; Beuls, Katrien and Michael Spranger. (Accepted). Russian Verbs of Motion and their Aspectual Partners in Fluid Construction Grammar. Constructions and Frames.")))))))
      (push `("*russian-motion-grammar*" . ,reference-text)  *reference-texts*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar-specific code     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you can add any code that you want to execute on the startup of your grammar.
;; (you might want to switch to your a different package here)
