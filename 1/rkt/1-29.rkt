#lang sicp
; #lang racket/base
; (require racket/trace)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define (simpson f a b n)
  (define (inc-2 n) (+ n 2))
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (* (/ h 3.0) 
     (+ (yk 0)
        (* 2 (sum yk 1 inc-2 (- n 1)))
        (* 4 (sum yk 2 inc-2 (- n 1)))
        (yk n))))


(define (cube n) (* n n n))
(integral cube 0 3 .00001)
(simpson cube 0 3 10000)

; (define (simpsonbad f a b n)
;   (define h (/ (- b a) n))
;   (define (y_k k) 
;     (if (even? k)
;         (* 2 (f (+ a (* k h))))
;         (* 4 (f (+ a (* (inc k) h))))))
;   (* (/ h 3.0) 
;      (+ (y_k 0)
;         (sum y_k 1 inc (- n 1))
;         (y_k n))))