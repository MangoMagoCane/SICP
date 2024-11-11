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

(define (eval-or exps env)
  (let (curr-eval (eval (first-exp exps) env))
    (if curr-eval
        curr-eval
        (if (last-exp? exps)
            'false
            (eval-or (rest-exps exps) env)))))

(define (eval-and-transform exps env)
  (define (and->if exp)
    (make-if (first-exp exp)
             (if (last-exp? exp)
                 (first-exp exp)
                 (and->if (rest-exps exp)))
             'false))
  (eval (and->if exps) env))

(define (eval-or-transform exps env)
  (define (or->if exp)
    (make-if (first-exp exp)
             (first-exp exp)
             (if (last-exp? exp)
                 'false
                 (or->if (rest-exps exp)))))
  (eval (and->if exps) env))

