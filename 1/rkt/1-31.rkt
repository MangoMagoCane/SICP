#lang sicp
; #lang racket/base
; (require racket/trace)

(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (wallis-pi n)
  (define (term x)
    ((lambda (x) (/ x (- x 1)))
     ((lambda (x) (* 4 (square x))) x)))
  (* 2 (product-iter term 1.0 inc n)))

(wallis-pi 10000000)

; (define (identity x) x)
; (product-rec identity 1 inc 5)
; (product-iter identity 1 inc 5)
; (define (inc-2 n) (+ n 2))
; (define (double x) (* x x))
; (define (pi-approxbad n)
;   (* 8.0 (/ (product-iter double 4 inc-2 (+ n 3))
;             (product-iter double 3 inc-2 (+ n 3)))))
; (pi-approxbad 100000)