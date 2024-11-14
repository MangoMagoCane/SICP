#lang sicp

(define (for-body exp) (cddddr exp))
(define (for-initialize exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-increment exp) (cadddr exp))

(define (for->combination exp)
  (make-named-let 
    '_RESERVED_FOR
    (list (for-initialize exp))
    (make-if (for-predicate exp)
              (make-begin 
                (append (for-body exp)
                        (list (make-application '_RESERVED_FOR
                                          (list (for-increment exp))))))
            '())))

