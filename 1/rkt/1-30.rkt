#lang sicp
; #lang racket/base
; (require racket/trace)

(define (sum term a next b)
  ; (trace sum)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    ; (trace iter)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-ints-rec a b)
  (sum identity a inc b))
(define (sum-ints-iter a b)
  (sum-iter identity a inc b))


(sum-ints-rec 1 100)
(sum-ints-iter 1 100)