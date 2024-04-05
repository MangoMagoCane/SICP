#lang sicp
; #lang racket/base
; (require racket/trace)

(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))

; k = 11 to match first 4 digits 
(cont-frac-rec (lambda (i) 1.0)
               (lambda (i) 1.0)
               11)
(cont-frac-iter (lambda (i) 1.0)
               (lambda (i) 1.0)
               11)

