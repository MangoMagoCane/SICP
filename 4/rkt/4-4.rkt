#lang sicp

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exps env)
  (define (inner exps)
    (let ((curr-eval (eval (first-exp exps) env)))
      (if curr-eval
          (if (last-exp? exps)
              curr-eval
              (inner (rest-exps exps) env))
          'false)))
  (inner (rest-exps exps)))

(define (eval-or exps env)
  (define (inner exps)
    (let ((curr-eval (eval (first-exp exps) env)))
      (if curr-eval
          curr-eval
          (if (last-exp? exps)
              'false
              (inner (rest-exps exps) env)))))
  (inner (rest-exps exps)))

(define (eval-and-transform exps env)
  (define (and->if exp)
    (make-if (first-exp exp)
             (if (last-exp? exp)
                 (first-exp exp)
                 (and->if (rest-exps exp)))
             'false))
  (eval (and->if (rest-exps exps)) env))

(define (eval-or-transform exps env)
  (define (or->if exp)
    (make-if (first-exp exp)
             (first-exp exp)
             (if (last-exp? exp)
                 'false
                 (or->if (rest-exps exp)))))
  (eval (and->if (rest-exps exps)) env))

