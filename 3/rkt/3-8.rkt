#lang sicp

(define f 
  (let ((ret-val #f))
    (lambda (x)
      (if ret-val
        0 
        (begin 
          (set! ret-val x)
          ret-val)))))

(define (f-log x)
  (display x)
  x)

(+ (f 0) (f 1))
(+ (f-log 0) (f-log 1))
