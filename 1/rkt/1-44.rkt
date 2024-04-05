#lang sicp
; #lang racket/base
; (require racket/trace)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (compose result f))))
  (iter 1 f))

(define dx 0.00001)
(define (smooth f dx)
  (lambda (x) 
    (/ (+ (f (- x dx)) 
          (f x) 
          (f (+ x dx))) 
       3)))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))