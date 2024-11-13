#lang sicp

(define (make-let* clauses body)
  (list 'let* clauses body))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (let*->nested-lets exp)
  (define (expand-clauses clauses)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-let (list first) (let*-body exp))
          (make-let (list first) (expand-clauses rest)))))
  (expand-clauses (let*-clauses exp)))

