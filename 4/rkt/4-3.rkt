#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else 
          (let ((proc (get 'eval (operator exp))))
            (if proc
                (proc exp env)
                (if (application? exp)
                    (apply (eval (operator exp) env)
                           (list-of-values (operands exp) env))
                    (error "Unknown expression type: EVAL" exp)))))))
