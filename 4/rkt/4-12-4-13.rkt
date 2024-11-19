#lang sicp

(define (lookup-variable-value var env)
  (env-loop var car (lambda (_ vals) (car vals)) env))

(define (set-variable-value! var val env)
  (env-loop var car (lambda (_ vals) (set-car! vals val)) env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan var
          (lambda (vals) (add-binding-to-frame! var val frame))
          car
          (lambda (_ vals) (set-car! vals val))
          frame)))

(define (scan var null-proc eq-cond eq-proc frame)
  (define (inner vars vals)
    (cond ((null? vars) (null-proc vals))
          ((eq? var (eq-cond vars)) (eq-proc vars vals))
          (else (inner (cdr vars) (cdr vals)))))
  (inner (frame-variables frame) (frame-values frame)))

(define (env-loop var eq-cond eq-proc env)
  (define (inner env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan var
              (lambda (_) (inner (enclosing-environment env)))
              eq-cond
              eq-proc
              (first-frame env))))
  (inner env))

(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (let ((vars (frame-variables frame))
          (vals (frame-values frame)))
      (if (eq? var (car vars))
          (begin (set-car! frame (cdr vars))
                 (set-cdr! frame (cdr vars)))
          (scan cadr
                (lambda (_) #f)
                (lambda (vars vals)
                  (set-cdr! vars (cddr vars))
                  (set-cdr! vals (cddr vals)))
                frame)))))

(define (make-unbound-deep! var env)
  (define (inner env)
    (cond ((make-unbound! var env) #t)
          ((eq? env the-empty-environment) #f)
          (else (inner var (enclosing-environment env)))))
  (inner env))

