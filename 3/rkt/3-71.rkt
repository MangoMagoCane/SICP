#lang sicp

; a
(weighted-pairs integers integers
                (lambda (x) (+ (car x) (cadr x)))
; b
(define ints-235 
  (stream-filter (lambda (x) 
    (not (or (divisible? x 2)
             (divisible? x 3)
             (divisible? x 5))))
    integers))

(weighted-pairs ints-235 ints-235 
(lambda (x) 
  (let ((i (car x))
        (j (cadr x)))
    (+ (* 2 i) 
        (* 3 j)
        (* 5 i j))))
