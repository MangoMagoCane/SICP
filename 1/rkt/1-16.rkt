#lang sicp

(define (square x) (* x x))

(define (fast-expt b n)
(cond ((= n 0) 1)
      ((even? n) (square (fast-expt b (/ n 2))))
      (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter b n a)
    ())
  
  (iter b n 1))

(fast-expt 4 4)
(fast-expt-iter 4 4)
