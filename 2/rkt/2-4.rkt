#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(define (num-pair x y)
  (cons x y))

(define pair (num-pair 1 2))
; (define pair (cons 1 2))
; (define pair (lambda (m) (m 1 2)))

(cdr pair)
(car pair)
; (pair (lambda (p q) p))
; ((lambda (m) (m 1 2)) (lambda (p q) p))
; ((lambda (p q) p) 1 2)
; 1