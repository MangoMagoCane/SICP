#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; O(nlogn)
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))
; O(n)
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

; Both have the same output
(define list1 `(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define list2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define list3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 list1)
(tree->list-2 list1)

(tree->list-1 list2)
(tree->list-2 list2)

(tree->list-1 list3)
(tree->list-2 list3)