#lang sicp

; recursive
(define (fr n)
  (cond ((< n 3) n)
        (else (+ (fr (- n 1))
                 (* 2 (fr (- n 2)))
                 (* 3 (fr (- n 3)))))))

(fr 7) ; 11
; (+ (fr 3) (* 2 (fr 2)) (* 3 (fr 1)))
; (+ (+ 2 2 0) (* 2 (fr 2)) (* 3 (fr 1)))
; (+ 4 4 3)

; iterative
(define (fi n)
  (define (fi-iter a b c count)
    (if (< count 3)
        a
        (fi-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
  (fi-iter 2 1 0 n))

(fi 7)