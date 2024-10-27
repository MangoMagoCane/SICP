#lang sicp

(define (make-accumulator initial)
  (lambda (sum)
    (set! initial (+ initial sum))
    initial))

(define A (make-accumulator 5))
(A 10)
(A 10)

