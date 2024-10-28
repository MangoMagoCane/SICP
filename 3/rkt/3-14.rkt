#lang sicp

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
    (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
; mystery is a stateful varient of append
; v (a)
; w (d c b a)

