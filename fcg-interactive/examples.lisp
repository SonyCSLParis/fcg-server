(in-package :fcg-server)

;; This file contains prototype code that was developed for research purposes and should not be used in production environments.
;; No warranties are provided.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      ;;
;; Loading examples for FCG interactive ;;
;;                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *example-utterances* '((fcg:*fcg-constructions* . ("the linguist likes the mouse"
                                                                 "the mouse likes the linguist"
                                                                 "the mouse"
                                                                 "the linguist"))
                                     ;; Add example utterances for your grammars here below:
                                     ;; e.g. (package::*grammar* . ("u1" "u2"))
                                     ))
  
(defparameter *example-meanings* '((fcg:*fcg-constructions* . ("mouse(x), linguist(y), unique(x), unique(y), deep-affection(y,x)"
                                                               "mouse(x), linguist(y), unique(x), unique(y), deep-affection(x,y)"
                                                               "mouse(x), unique(x)"
                                                               "linguist(x), unique(x)"))
                                   ;; Add example meanings for your grammars here below:
                                   ;; e.g. (package::*grammar* . ("m1" "m2"))
                                   ))