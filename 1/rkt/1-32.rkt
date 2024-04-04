#lang sicp
; #lang racket/base
; (require racket/trace)

; (define (product-iter term a next b)
;   (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (* result (term a)))))
;   (iter a 1))

; (define (sum-iter term a next b)
;   (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (+ result (term a)))))
;   (iter a 0))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (summation-iter term a next b)
  (accumulate-iter + 0 term a next b))
(define (summation-rec term a next b)
  (accumulate-rec + 0 term a next b))
(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (inc n) (+ n 1))
(define (identity x) x)

(product-iter identity 1 inc 5)
(summation-iter identity 1 inc 100)
(summation-rec identity 1 inc 100)
