#lang sicp

(define (stream-limit s t)
  (define (inner s)
    (let* ((cdr-s (stream-cdr s))
           (cadr-s (stream-car s)))
      (if (< (abs (- cadr-s (stream-car s))) t)
          cadr-s
          (inner cdr-s))))
  (inner s))
