#lang sicp

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; normal-order evaluation
; (if (= 0 0) 
;     0 
;     (p))
;
; applicative-order evaluation
; evaluate the expressions test, 0, and (p)
; test never is evaluated since (p) calls itself recursively indefinetly   
; 
