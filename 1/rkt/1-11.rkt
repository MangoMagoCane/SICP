#lang sicp

; recursive
(define (f-r n)
  (cond ((< n 3) n)
        (else (+ (f-r (- n 1))
                 (* 2 (f-r (- n 2)))
                 (* 3 (f-r (- n 3)))))))

(f-r 3)
; iterative
(define (f-i n)
  ())