#lang sicp

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
(last-pair z)
; computer (last-pair z) will loop infinitely because 
; the last cdr of z points back to itself and never nil
