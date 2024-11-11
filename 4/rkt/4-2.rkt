#lang sicp

; a
; the problem with Louis Reasoner's plan to move
; the procedure application clause before that of
; assignment is that calling define will no longer
; be treated as a special form, instead resulting in an
; enviornment lookup failure

; b
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        (else
          (error "Unknown expression type: EVAL" exp))))
