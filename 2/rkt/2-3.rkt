#lang sicp

; numbers
(define (square x) (* x x))

; points
(define (make-point x y) (cons x y))
(define (x-point p) (car p)) ; x-point should really be named something like x-coord
(define (y-point p) (cdr p))
(define (distance-points p1 p2)
  (sqrt (+ (square (- (x-point p2) (x-point p1))) 
           (square (- (y-point p2) (y-point p1))))))

; rectangles
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))
(define (bl-rect rect) (car rect))
(define (tr-rect rect) (cdr rect))
(define (br-rect rect) 
  (make-point (x-point (bl-rect rect)) 
              (y-point (tr-rect rect))))
(define (tl-rect rect) 
  (make-point (x-point (tr-rect rect))
              (y-point (bl-rect rect))))

; functions
(define (perimeter rect)
  (+ (* 2 (distance-points (bl-rect rect) (br-rect rect)))
     (* 2 (distance-points (tl-rect rect) (tr-rect rect)))))
(define (area rect)
  (* (distance-points (bl-rect rect) (br-rect rect))
     (distance-points (br-rect rect) (tr-rect rect))))

(define test (make-rect (make-point 0 0) (make-point 1 1)))
(perimeter test)
(area test)