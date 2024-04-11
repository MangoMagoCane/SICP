#lang sicp

(define (square x) (* x x))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (tree-map-rec proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map-rec proc (car tree))
                    (tree-map-rec proc (cdr tree))))))

(define (square-tree tree) (tree-map square tree))

(square-tree
  (list 1
    (list 2 (list 3 4) 5)
    (list 6 7)))