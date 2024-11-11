#lang sicp

(define (and-or-actions exp) (cdr exp))
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exps env)
  (let (curr-eval (eval (first-exp exps) env))
    (if curr-eval
        (if (last-exp? exps)
            curr-eval
            (eval-and (rest-exps exps) env))
        'false)))

(define (eval-and exps env)
  (define (and->if exp)
    (make-if (first-exp exp)
             (if (last-exp? exp)
                 (first-exp exp)
                 (and->if (rest-exps exp)))
             'false))
  (eval (and->if exps) env))

(define (eval-or exps env)
  (define (or->if exp)
    (make-if (first-exp exp)
             (first-exp exp)
             (if (last-exp? exp)
                 'false
                 (or->if (rest-exps exp)))))
  (eval (and->if exps) env))

(define (eval-or exps env)
  (let (first-eval (eval (first-exp exps) env))
    (if first-eval
        first-eval
        (if (last-exp? exps)
            'false
            (eval-or (rest-exps exps) env)))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (and-or-actions exp) env))
        ((or? exp) (eval-or (and-or-actions exp) env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))
