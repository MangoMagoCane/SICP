#lang sicp

(define (make-interval a b) (cons a b))
; 2-7
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (width-interval x) 
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
; 2-8
(define (sub-interval x y)
  (add-interval 
   x
   (make-interval (- (upper-bound y))
                  (- (lower-bound y)))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
; 2-10
(define (div-interval x y)
  (let ((upper-y (upper-bound y))
        (lower-y (lower-bound y)))
    (if (and (> upper-y 0) (> lower-y 0)) ; (> (* upper-y lower-y) 0)
        (mul-interval 
         x
         (make-interval (/ 1.0 upper-y)
                        (/ 1.0 lower-y)))
        (error "division by interval that spans zero"))))
; (div-interval (make-interval 1 2) (make-interval -1 3))

; 2-9
; (define intval1 (make-interval 1 2))
; (define intval2 (make-interval 1 4))

; (+ (width-interval intval1) (width-interval intval2))
; (width-interval (add-interval intval1 intval2))

; (* (width-interval intval1) (width-interval intval2))
; (width-interval (mul-interval intval1 intval2))

; 2-12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (midpoint interval)
  (- (upper-bound interval) (width interval)))

(define (make-center-percent c pt)
  (make-center-width c (* pt c)))
(define (percent interval)
  (/ (width interval) (midpoint interval)))
