#lang sicp
; #lang racket/base
; (require racket/trace)

(define (double f) 
  (lambda (x) (f (f x))))

; runs 16 times 
(define (inc x) (+ x 1))
(((double (double double)) inc) 5)
(((double double) ((double double) inc)) 5)
(((double double) (double (double inc))) 5)
; (((double (double (double (double inc))))) 5)
