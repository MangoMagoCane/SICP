#lang sicp

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines-simultaneous body) env))

(define (lookup-variable-value var env)
  (define invalid-symbols '(*unassigned*))
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
          (frame-values frame)))))
  (let ((invalid-symbol? (memq var invalid-symbols)))
    (if invalid-symbol?
        (error "Attempted lookup of symbol" (car invalid-symbol?))
        (env-loop env))))

(define (scan-out-defines body proc)
  (define (scan exps defns)
    (let ((curr-exp (first-exp exps)))
      (if (definition? curr-exp)
          (scan
            (rest-exps exps)
            (cons (list (definition-variable curr-exp)
                        (definition-value curr-exp))
                  defns))
          (proc defns exps))))
  (if (definition? (first-exp body))
      (scan body '())
      body))

(define (scan-out-defines-let-set body)
  (scan-out-defines
    body
    (lambda (defns rest-of-body)
      (let ((defns-correct-order (reverse defns)))
        (list
          (make-let
            (map
                (lambda (exp)
                  (list (car exp)
                        ''*unassigned*))
                defns-correct-order)
            (append
              (map
                (lambda (exp)
                  (list 'set!
                        (car exp)
                        (cadr exp)))
                defns-correct-order)
              rest-of-body)))))))
