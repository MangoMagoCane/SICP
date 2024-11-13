#lang sicp

(define (make-let clauses body)
  (list 'let clauses body))

(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define (let->combination exp)
  (define (expand-clauses clauses vars exps)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-application
            (make-lambda (reverse (cons (let-var first) vars))
                         (let-body exp))
            (list (reverse (cons (let-exp first) exps))))
          (expand-clauses rest 
                          (cons (let-var first) vars) 
                          (cons (let-exp first) exps)))))
  (expand-clauses (let-clauses exp) '() '()))
