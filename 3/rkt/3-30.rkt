#lang sicp

(define (ripple-carry-adder A B S C)
  (define (inner A B S C-in)
    (if null? A)
        (set-signal! C-in 0)
        (let ((C-out (make-wire)))
          (full-adder (car A) (car B) C-in (car S) C-out)
          (inner (cdr A) (cdr B) (cdr S) C-out)))
  (inner A B S C))
