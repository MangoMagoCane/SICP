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

(define (square x) (* x x))
((repeated square 2) 5)
