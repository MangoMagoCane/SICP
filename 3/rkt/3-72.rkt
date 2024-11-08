#lang sicp

(define (sum-triple x)
  (+ (expt (car x) 3)
     (expt (cadr x) 3)))

(define (raman-filter stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((= (sum-triple (stream-car stream))
            (sum-triple (stream-car (stream-cdr stream))))
          (cons-stream (append (stream-car stream) (stream-car (stream-cdr stream)))
                       (raman-filter (stream-cdr stream))))
        (else (raman-filter (stream-cdr stream)))))

(define ramanujan-numbers
  (raman-filter
    (weighted-pairs integers integers sum-triple)))

(stream-ref ramanujan-numbers 5)

