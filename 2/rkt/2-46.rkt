#lang sicp

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect xvect yvect)
  (make-vect (+ (xcor-vect xvect)
                (xcor-vect yvect))
             (+ (ycor-vect xvect)
                (ycor-vect yvect))))

(define (sub-vect xvect yvect)
  (make-vect (- (xcor-vect xvect)
                (xcor-vect yvect))
             (- (ycor-vect xvect)
                (ycor-vect yvect))))

(define (scale-vect vect s)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))

(define a (make-vect 1 2))
(define b (make-vect 3 4))

(add-vect a b)
(sub-vect a b)
(scale-vect a 5)