#lang sicp

(define (make-application procedure parameters)
  (cons procedure parameters))
(define (cond-arrow-action clause) (caddr clause))

(define (cond-arrow-clause? clause)
  (tagged-list? (cond-actions clause) '=>))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if (cond-arrow-clause? first)
                (make-application
                  (make-lambda '(_RESERVED_COND)
                               (make-if '_RESERVED-COND
                                        (make-application (cond-arrow-action first)
                                                          '(_RESERVED-COND))
                                        (expand-clauses rest)))
                  (cond-predicate first))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
