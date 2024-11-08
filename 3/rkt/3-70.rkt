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
                        (merge-weighted (stream-cdr s1) s2 weight)))
                    ((> s1weight s2weight)
                      (cons-stream
                        s2pair
                        (merge-weighted s1 (stream-cdr s2) weight)))
                    (else 
                      (cons-stream
                        s1pair 
                        (cons-stream
                          s2pair
                          (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))))))))))
 
(define (weighted-pairs s t proc)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) proc)
      proc)))

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

