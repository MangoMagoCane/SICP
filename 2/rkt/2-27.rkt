#lang sicp

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(define (deep-reverse items)
  (if (not (pair? items)) 
      items
      (reverse (cons (deep-reverse (car items))
                     (deep-reverse (cdr items))))))

(define (deep-reverse2 items)
  (if (not (pair? items)) 
      items
      (reverse (map deep-reverse2 items))))

(define x (list (list 1 2) (list 3 4)))
; x
; ((1 2) (3 4))
; (reverse x)
; ((3 4) (1 2))
; (deep-reverse2 x) 
; ((4 3) (2 1))