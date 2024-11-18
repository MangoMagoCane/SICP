#lang sicp

(define (lookup-variable-value var env)
  (env-loop (first-frame env) (lambda () (car vals)) env))

(define (set-variable-value! var val env)
  (env-loop (lambda () (set-car! vals val)) env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
          (frame-values frame)
          (lambda () (add-binding-to-frame! var val frame))
          (lambda () (set-car! vals val))

(define (scan vars vals null-cond eq-cond)
  (define (inner vars vals)
    (cond ((null? vars)
            (null-cond)
          ((eq? var (car vars)) (eq-cond))
          (else (inner (cdr vars) (cdr vals))))))
  (inner vars vals))

(define (env-loop frame eq-cond env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame))
              (lambda () (env-loop (enclosing-environment env)))
              eq-cond)))

