#lang sicp

(define (list-of-values-l-to-r exps env)
  (if (no-operands? exps)
      '()
      (let ((l-val (eval (first-operand exps) env)))
        (let ((r-val (list-of-values (rest-operands exps) env)))
          (cons l-val r-val)))))

(define (list-of-values-r-to-l exps env)
  (if (no-operands? exps)
      '()
      (let ((r-val (list-of-values (rest-operands exps) env)))
        (let ((l-val (eval (first-operand exps) env)))
          (cons l-val r-val)))))


