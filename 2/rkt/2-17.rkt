#lang sicp

(define (last-pair ls)
    (if (null? (cdr ls))
        (car ls)
        (last-pair (cdr ls))))

(last-pair (list 23 72 149 34))
