#lang sicp

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define cyc (make-cycle (list 'a 'b 'c)))
(define non-cyc (list 'a 'b 'c))

(define (cycle? l)
  (define (inner l prev-pairs)
    (cond ((null? l) #f)
          ((memq l prev-pairs) #t)
          (else (inner (cdr l) (cons l prev-pairs)))))
  (inner l '()))

(define (cycle-const? ls)
  (define (inner p1 p2 mv-pntr)
    (cond ((null? p2) #f)
          ((eq? p1 p2) #t)
          (else
            (if mv-pntr
                (inner (cdr p1) (cdr p2) #f)
                (inner p1 (cdr p2) #t)))))
  (and (not (null? ls)
       (inner ls (cdr ls) #f)))

(cycle? cyc)
(cycle? non-cyc)

(cycle-const? cyc)
(cycle-const? non-cyc)
