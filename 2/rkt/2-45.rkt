#lang sicp

(define (beside _p1 _p2) 1)
(define (below _p1 _p2) 1)

(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  iter)

(define right-split (split beside below))
(define up-split (split below beside))
