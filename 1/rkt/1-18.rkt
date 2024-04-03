; #lang sicp
#lang racket/base
(require racket/trace)

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (log-mult-iter a b)
  (define (iter n a b)
  (trace iter)
    (cond ((= b 0) n)
          ((even? b) (iter n (double a) (halve b)))
          (else (iter (+ n a) a (- b 1)))))
  (iter 0 a b))

(log-mult-iter 17 19)