#lang sicp
; #lang racket/base
; (require racket/trace)

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        (+ result 0.0)
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (tan-cf-abstraction x k)
  (cont-frac-iter 
    (lambda (i) (if (= i 1) x (* x x -1)))
    (lambda (i) (- (* 2 i) 1))
    k))

(define (tan-cf x k)
  (define (d i) (- (* 2 i) 1))
  (let ((x^2 (* x x)))
    (define (iter i result)
      (if (= i 1)
          (/ (+ x 0.0) (- 1 result))
          (iter (- i 1) (/ x^2 (- (d i) result)))))
    (iter (- k 1) (/ x^2 (d k)))))
  

(tan-cf 20 100)
(tan-cf-abstraction 20 100)
(tan 20)