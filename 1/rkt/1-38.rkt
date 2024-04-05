#lang sicp
; #lang racket/base
; (require racket/trace)

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

(define (d-euler i)
  (if (= (modulo i 3) 2)
      (* 2 (/ (+ i 1) 3))
      1))

(define (e-approx k)
  (+ 2 (cont-frac-iter (lambda (i) 1.0) d-euler k)))

(e-approx 100)