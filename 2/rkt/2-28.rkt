#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) 
                      (fringe (cdr items))))))
  
(define x (list (list 1 2) (list 3 4)))
(fringe x)
; (1 2 3 4)
(fringe (list x x))
; (1 2 3 4 1 2 3 4)