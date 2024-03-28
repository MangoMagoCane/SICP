#lang sicp

; if b is a positive number, subract b from a, else add b and a
; essentially add the absolute value of b to a
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) 

(a-plus-abs-b 1 -4)
