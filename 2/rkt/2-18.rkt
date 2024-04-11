#lang sicp

(define nil '())

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse0 ls)
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls) (append (list (car ls)) result))))
  (iter ls nil))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (cons (car items) result))))
  (iter items nil))

(reverse (list 1 4 9 16 25))