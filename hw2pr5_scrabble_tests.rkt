#lang racket

(require rackunit)
(require "hw2pr5_scrabble.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing for subbag? ;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;; provided tests
(check-true  (subbag? '()      '(s p a m s)) )
(check-true  (subbag? '(s s)   '(s p a m s)) )
(check-true  (subbag? '(s m)   '(s p a m s)) )
(check-true  (subbag? '(a p)   '(s p a m s)) )
(check-false (subbag? '(a m a) '(s p a m s)) )
(check-true  (subbag? '(a s)   '(s a))       )

;; student tests
(check-true (subbag? '() '()))
(check-true (subbag? '() '(h e l l o)))
(check-false (subbag? '(h e l l e) '(h e l l o)))
(check-false (subbag? '(a b c d) '(e f g h)))
(check-true (subbag? '(a b c d) '(a b c d e)))
(check-false (subbag? '(a b c d e) '(a b c d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing for best-word ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; provided tests
(check-equal? (best-word "academy" '("ace" "ade" "cad" "cay" "day")) 
 '("cay" 8))
(check-equal? (best-word "appler"  '("peal" "peel" "ape" "paper")) 
 '("paper" 9))
(check-equal? (best-word "paler"   '("peal" "peel" "ape" "paper"))
 '("peal" 6))
(check-equal? (best-word "kwyjibo" '("ace" "ade" "cad" "cay" "day"))
 '("" 0))

;; student tests

(check-equal? (best-word "hi" '("hi")) '("hi" 5))
(check-equal? (best-word "it" '("hay" "the" "rained" "clay" "pow"))
 '("" 0))
(check-equal? (best-word "" '("hay" "the" "rained" "clay" "pow"))
 '("" 0))
