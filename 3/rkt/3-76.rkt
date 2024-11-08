#lang sicp

(define (smooth stream)
  (stream-map
    (lambda (x y) (/ (+ x y) 2)
    stream
    (stream-cdr stream))))

(define zero-crossings
  (let ((smooth-sense-data (cons-stream 0 (smooth sense-data))))
    (stream-map
      sign-change-detector
      (stream-cdr smooth-sense-data)
      smooth-sense-data)))
