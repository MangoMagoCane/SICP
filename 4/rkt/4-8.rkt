#lang sicp

(define (make-let clauses body)
  (cons 'let (cons clauses body)))
(define (make-named-let var bindings body)
  (cons 'let (cons var bindings body)))

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
                         (let-body exp))
            (reverse (cons (let-exp first) exps)))
          (expand-clauses rest
                          (cons (let-var first) vars)
                          (cons (let-exp first) exps)))))
  (define (expand-named-let exp)
    (let ((var (named-let-var exp))
          (bindings (named-let-bindings exp)))
      (make-application
        (make-lambda
          '()
          (list
            (make-definition
              (cons var (map let-var bindings))
              (named-let-body exp))
            (cons var (map let-exp bindings))))
          '())))
  (if (named-let? exp)
      (expand-named-let exp)
      (expand-clauses (let-clauses exp) '() '())))

