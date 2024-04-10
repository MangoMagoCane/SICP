#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse ls)
  (define (iter ls result)
    (if (null? ls)
        result
        (iter (cdr ls) (append (list (car ls)) result))))
  (iter ls nil))

(define (same-parity . ls) 
  (let ((parity (if (even? (car ls))
                    even?
                    odd?)))
    (define (iter ls result)
      (if (null? ls)
          result
          (iter (cdr ls)
                (if (parity (car ls))
                    (append (list (car ls)) result)
                    result))))
    (reverse (iter ls nil))))

(same-parity 1 2 3 4 5 6 7)