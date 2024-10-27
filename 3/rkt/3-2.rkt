#lang sicp

(define (make-monitored func)
  (let ((count 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          count
          (begin
            (set! count (+ count 1))
            (func arg))))))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

(define c (make-monitored (lambda (x) (+ x 1))))
(c 100)
(c 'how-many-calls?)
(c 101)
(c 1)
(c 'how-many-calls?)
