#lang sicp

(define (square x) (* x x))

; produces in reverse order previous anser is the second element of every construct
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; doesn't work because car at the head of the list gives the nil list that was passed in first
(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))