#lang sicp

(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

; memoized
(square (id 10)) ; 100
count ; 1

; not memoized
(square (id 10)) ; 100
count ; 2
