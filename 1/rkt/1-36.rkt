#lang sicp
; #lang racket/base
; (require racket/trace)

(define (average x y) (/ 2 (+ x y)))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess step-count)
    ; (display guess)
    ; (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          ((lambda (next) (display step-count) (newline) next) next)
          (try next (+ step-count 1)))))
  (try first-guess 1))


; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

