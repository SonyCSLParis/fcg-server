(in-package :fcg-server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load system for the grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op 'portuguese-grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to cxn-inventory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here, you should add a pointer to a folder containing static html for your cxn-inventory
;; You can make it with the function make-html-cxn-inventory-for-fcg-interactive , e.g.
;; (fcg::make-html-cxn-inventory-for-fcg-interactive fcg::*portuguese-corpus-grammar* :hide-labels '(fcg::morph fcg::lex))
;; Then put it somewhere /var/www/wordpress/FCG/cxn-inventories - or talk to paul

(pushnew '("*portuguese-corpus-grammar*" . "cxn-inventories/portuguese-corpus-grammar/") *links-cxn-inv*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to examples     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you should add links to text files containing example sentences/meanings

(push (cons "*portuguese-corpus-grammar*" (read-example-sentences
                                              (babel-pathname  :directory '("grammars" "Portuguese")
                                                               :name "correct-sentences" :type "txt")))
      *example-sentences*)

(push (cons "*portuguese-corpus-grammar*" (read-example-meanings
                                              (babel-pathname  :directory '("grammars" "Portuguese")
                                                               :name "correct-meanings" :type "txt")))
      *example-meanings*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add pointer to reference text  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let ((reference-text
       (string-append (render-xml '((div :class "ref_web_demo")((p)"For a documented web demonstration supporting this grammar, click "
                                    ((a :href "https://www.fcg-net.org/demos/propor-2016") "here."))))
                      (render-xml '((div :class "ref_supporting_publications")
                                    ((p) "Supporting publications:")
                                    ((ul)
                                     ((li)"Marques, T&acirc;nia & Beuls, Katrien. (2016). A Construction Grammar Approach for Pronominal Clitics in European Portuguese. Computational Processing of the Portuguese Language (PROPOR 2016).")
                                     ((li)"Marques, T&acirc;nia & Beuls, Katrien. (2016). Evaluation Strategies for Computational Construction Grammars.The 26th International Conference on Computational Linguistics (COLING 2016).")))))))
      (push `("*portuguese-corpus-grammar*" . ,reference-text)  *reference-texts*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar-specific code     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here you can add any code that you want to execute on the startup of your grammar.
;; (you might want to switch to your a different package here)

(in-package :fcg)

(defparameter *portuguese-corpus-grammar* (create-grammar))



