#lang sicp

(define (make-let clauses body)
  (list 'let clauses body))
(define (make-named-let var bindings body)
  (list 'let var bindings body))

(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (not (pair? (named-let-var exp))))

(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))

(define (let->combination exp)
  (define (expand-clauses clauses vars exps)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-application
            (make-lambda (reverse (cons (let-var first) vars))
                         (list (let-body exp)))
            (reverse (cons (let-exp first) exps)))
          (expand-clauses rest
                          (cons (let-var first) vars)
                          (cons (let-exp first) exps)))))
  (define (expand-named-let exp)
    (let ((var (named-let-var exp))
          (bindings (named-let-bindings exp)))
      (make-let 
        (list (list var (make-lambda
                          (map let-var bindings)
                          (list (named-let-body exp)))))
        (make-application var (map let-exp bindings)))))
  (if (named-let? exp)
      (expand-named-let exp)
      (expand-clauses (let-clauses exp) '() '())))

