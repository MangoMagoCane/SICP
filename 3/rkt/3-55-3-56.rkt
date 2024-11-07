#lang sicp

(define (square x) (* x x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (begin
        (stream-car s)
        (display (stream-car s)) (newline))
      (begin 
        (display (stream-car s)) (display " ")
        (stream-ref (stream-cdr s) (- n 1)))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter
                         pred
                         (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (set! val (+ val 1))
             (display val) (newline)
             (stream-for-each proc (stream-cdr s)))))

(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers
  (integers-starting-from 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-55

(define (partial-sums S) ; (1 2 3 4 5) (1 3 6 10 15)
  (define ps 
    (cons-stream (stream-car S) 
                 (add-streams (stream-cdr S) ps)))
  ps)

#| (stream-ref (partial-sums (integers-starting-from 1)) 5) |#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                    (cons-stream
                      s2car
                      (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))

(define S 
  (cons-stream 
    1 
    (merge 
      (merge (scale-stream S 2) (scale-stream S 3)) 
      (scale-stream S 5))))

#| (stream-ref S 20) |#

