#lang sicp
; #lang racket/base
; (require racket/trace)

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; (define (filtered-accumulate filter combiner null-value term a next b)
;   (define (iter a result)
;     (cond ((> a b) result)
;           ((filter a) (iter (next a) (combiner result (term a))))
;           (else (iter (next a) result))))
;   (iter a null-value))
        
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (if (filter a) 
                  (combiner result (term a)) 
                  result))))
  (iter a null-value))

(define (identity x) x)
(define (inc n) (+ n 1))

(define (sum-even a b)
  (filtered-accumulate even? + 0 identity a inc b))

(sum-even 1 10)