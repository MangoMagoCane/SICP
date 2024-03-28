#lang sicp

(define (square x) (* x x))

(define (sum-square-largest-two x y z)
  (+  (square (cond ((or (> x y) (> x z)) x) (else 0)))
      (square (cond ((or (> y x) (> y z)) y) (else 0)))
      (square (cond ((or (> z y) (> z x)) z) (else 0)))))

(sum-square-largest-two 5 1 6)