#lang sicp

; RECURSIVE
; (+ 3 4)
; (inc (+ 2 4))
; (inc (inc (+ 1 4)))
; (inc (inc inc ((+ 0 4))))
; (inc (inc 5))
; (inc 6)
; 7
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

; ITERATIVE
; (+ 3 4)
; (+ 2 5)
; (+ 2 6)
; 7 
(define (_+ a b)
  (if (= a 0) b (_+ (dec a) (inc b))))
