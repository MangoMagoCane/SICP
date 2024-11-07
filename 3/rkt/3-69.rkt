#lang sicp

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)) 
      (interleave
        (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                    (stream-cdr u))
        (stream-map (lambda (x) (list (stream-car s) x (stream-car u)))
                    (stream-cdr t))))))

(define int-triples (triples integers integers integers))

(stream-for-each 
  (lambda (triple)
    (let ((x (car triple))
          (y (cadr triple))
          (z (caddr triple)))
    (if (or (< z y)
            (< z x)
            (< y x))
      (error "Invalid triple:" triple))))
  int-triples)
