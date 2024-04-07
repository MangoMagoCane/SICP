#lang sicp

(define (average x y) (/ (+ x y) 2.0))

(define (make-point x y) (cons x y))
(define (x-point p) (car p)) ; x-point should really be named something like x-coord
(define (y-point p) (cdr p))
(define (average-points p1 p2) 
  (make-point (average (x-point p1) (x-point p2))
              (average (y-point p1) (y-point p2))))

(define (make-segment start-p end-p) (cons start-p end-p))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (midpoint-segment seg)
  (average-points (start-segment seg) 
                  (end-segment seg)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define segment 
  (make-segment (make-point 0 0) 
                (make-point 1 2)))
(print-point (midpoint-segment segment))

; (define (midpoint-segment2 seg)
;   (let ((p1 (start-segment seg))
;         (p2 (end-segment seg)))
;   (make-point (average (x-point p1) 
;                        (x-point p2))
;               (average (y-point p1) 
;                        (y-point p2)))))