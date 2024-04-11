#lang sicp

(define (square x) (* x x))

(define (square-tree-map tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
         tree))

(define (square-tree-rec tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-rec (car tree))
                    (square-tree-rec (cdr tree))))))

(square-tree-map
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))
(square-tree-rec
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))
; (1 (4 (9 16) 25) (36 49))