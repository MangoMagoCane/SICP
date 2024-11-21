#lang sicp

(define (for? exp) (tagged-list exp 'for))
(define (for-initialize exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-increment exp) (cadddr exp))
(define (for-body exp) (cddddr exp))

(define (for->combination exp)
  (make-named-let
    '*reserved-for*
    (list (for-initialize exp))
    (make-if (for-predicate exp)
              (make-begin
                (append (for-body exp)
                        (list (make-application '*reserved-for*
                                          (list (for-increment exp))))))
            '())))

