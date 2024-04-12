#lang sicp

(define (make-mobile left right)
  (list left right))
(define (left-branch mobile) 
  (car mobile))
(define (right-branch mobile)  
  (car (cdr mobile)))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))



(define x (make-mobile (make-branch 1 2) (make-branch 3 4)))

(define (total-weight mobile)
  ())