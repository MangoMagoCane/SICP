#lang sicp

(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-a frame)
  (car frame))

(define (edge1-frame-a frame)
  (cadr frame))

(define (edge2-frame-a frame)
  (caddr frame))

(define foo (make-frame-a 1 2 3))

(origin-frame-a foo)
(edge1-frame-a foo)
(edge2-frame-a foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-b frame)
  (car frame))

(define (edge1-frame-b frame)
  (cadr frame))

(define (edge2-frame-b frame)
  (cddr frame))

(define bar (make-frame-b 1 2 3))

(origin-frame-b bar)
(edge1-frame-b bar)
(edge2-frame-b bar)