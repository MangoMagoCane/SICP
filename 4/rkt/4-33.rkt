#lang sicp

(define (text-of-quotation exp env)
  (define (rec-quote exp)
    (if (pair? exp)
        (list 'cons (rec-quote (car exp)) (rec-quote (cdr exp)))
        (list 'quote exp)))
  (if (pair? (cadr exp))
      (eval (rec-quote (cadr exp)) env)
      (cadr exp)))
