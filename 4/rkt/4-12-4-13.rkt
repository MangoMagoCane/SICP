#lang sicp

(define (lookup-variable-value var env)
  (env-loop car (lambda (_1 vals) (car vals)) env))

(define (set-variable-value! var val env)
  (env-loop car (lambda (_1 vals) (set-car! vals val)) env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (scan (lambda (_1 vals) (add-binding-to-frame! var val frame))
          car
          (lambda (_1 vals) (set-car! vals val))
          frame)))

(define (scan null-proc eq-cond eq-proc frame)
  (define (inner vars vals)
    (cond ((null? vars) (null-proc vals)
          ((eq? var (eq-cond vars)) (eq-proc vars vals))
          (else (inner (cdr vars) (cdr vals))))))
  (inner (frame-variables frame) (frame-values vals)))

(define (env-loop eq-cond eq-proc env)
  (define (inner env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (scan (lambda (_1 _2) (inner (enclosing-environment env)))
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
                (lambda (_1) #f)
                (lambda (vars vals)
                  (set-cdr! vars (cddr vars))
                  (set-cdr! vals (cddr vals)))
                frame)))))

(define (make-unbound-deep! var env)
  (define (inner env)
    (cond ((make-unbound! var ebv) #t)
          ((eq? env the-empty-environment) #f)
          (else (inner var (enclosing-environment env)))))
  (inner env))

