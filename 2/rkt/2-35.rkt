#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate 
    + 
    0 
    (map (lambda (t) 
            (cond ((null? t) 0)
                  ((not (pair? t)) 1)
                  (else (count-leaves t)))) 
          t)))

(count-leaves (list 1 (list 2 3) (list 4 5 (list 6 7))))