#lang sicp

(define (count-pairs-ben x)
  (if (not (pair? x))
    0
    (+ (count-pairs-ben (car x))
       (count-pairs-ben (cdr x))
       1)))

(define count-pairs
  (let ((prev-pairs '()))
    (define (inner x) 
      (if (or (not (pair? x)) 
              (memq x prev-pairs))
          0
          (begin 
            (set! prev-pairs (cons x prev-pairs))
            (+ (inner (car x))
               (inner (cdr x))
               1))))
    inner))


(define foo (cons 1 2))
(define bar (cons foo foo))
(define baz (list 1 2 3))
(set-cdr! (cddr baz) baz)

(define pairs-3 (cons (cons 1 2) (cons 1 2)))
(define pairs-5 (cons foo bar))
(define pairs-7 (cons bar bar))
(define pairs-loop baz)
#| (count-pairs-ben pairs-3) ; 3 |#
#| (count-pairs-ben pairs-5) ; 5 |#
#| (count-pairs-ben pairs-7) ; 7 |#
#| (count-pairs-ben pairs-loop) ; infinite! |#

#| (count-pairs pairs-3) |#
#| (count-pairs pairs-5) |#
#| (count-pairs pairs-7) |#
#| (count-pairs pairs-loop) |#
