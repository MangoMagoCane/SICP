#lang sicp

(define (square x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(fast-expt 2 32)
(fast-expt-iter 2 32)

; (fast-expt-iter 4 4)
; (fast-expt 4 4) ; 256
; (square (fast-expt 4 2))
; (square (square (fast-expt 4 1)))
; (square (square (* 4 (fast-expt 4 0))))
; (square (square (* 4 1)))
; (square (square 4))
; (square 16)
; 256