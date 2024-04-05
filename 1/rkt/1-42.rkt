#lang sicp
; #lang racket/base
; (require racket/trace)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))
((compose square inc) 6)