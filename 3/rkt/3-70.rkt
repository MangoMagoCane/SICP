#lang sicp

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else 
          (let ((s1pair (stream-car s1))
                (s2pair (stream-car s2)))
            (let ((s1weight (weight s1pair))
                  (s2weight (weight s2pair)))
              (cond ((< s1weight s2weight)
                      (cons-stream
                        s1pair
                        (merge (stream-cdr s1) s2)))
                    ((> s1weight s2weight)
                      (cons-stream
                        s2pir
                        (merge s1 (stream-cdr s2))))
                    (else 
                      (cons-stream
                        s1pair 
                        (cons-stream
                          s2pair
                          (merge (stream-cdr s1) (stream-cdr s2)))))))))))
