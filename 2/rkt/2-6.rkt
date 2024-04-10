#lang sicp
; barely did myself

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
; (lambda (f) (lambda (x) (f ((zes

(add-1 one)
; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
; (lambda (f) (lambda (x) (f (((lambda (x) (f x))) x))))
; (lambda (f) (lambda (x) (f ((f x)))))

(define (add a b)
  (lambda (f) 
    (lambda (x) 
      ((a f) ((b f) x)))))

(define (church-to-int cn) 
   ((cn (lambda (n) (+ n 1))) 0)) 

(church-to-int two)
