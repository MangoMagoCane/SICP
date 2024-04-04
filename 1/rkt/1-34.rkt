#lang sicp
; #lang racket/base
; (require racket/trace)

(define (square x) (* x x))
(define (f g) (g 2))

(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6

; evaluator will attempt to evaluate 2 as a procedure with argument 2
; (f f)
; (f 2)
; (2 2)