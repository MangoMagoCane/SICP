#lang sicp

(define operator car)
(define operands cdr)
(define (=number? exp num) (and (number? exp) (= exp num)))
(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
                (operands exp) var))))

(define (install-sum-package)
  (define addend car)
  (define augend cadr)
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
            (+ a1 a2))
          (else (list '+ a1 a2))))
  (put 'deriv '+
       (lambda (ops var) 
         (make-sum (deriv (addend ops) var)
                   (deriv (augend ops) var)))))

(define (install-product-package)
  (define multiplier car)
  (define multiplicand cadr)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (put 'deriv '* 
       (lambda (ops var)
         (make-sum 
           (make-product 
             (multiplier ops)
             (deriv (multiplicand ops) var))
           (make-product 
             (deriv (multiplier ops) var)
             (multiplicand ops))))))

(define (install-exponentiation-package)
  (define base car)
  (define exponent cadr)
  (define (make-exponentiation b e)
    (cond ((=number? b 1) 1)
          ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list `** b e))))
  (put 'deriv '**
       (lambda (ops var)
         (make-product
           (make-product 
             (exponent ops)
             (deriv (base ops) var))
           (make-exponentiation 
             (base ops)
             (make-sum (exponent ops) -1))))))

; a. mumber? and variable? cannot be assimilated because a data-directed dispatch
; is on not only on types, in this case 'deriv but also an operation, such as '+ or '*
; the predicates do not have an explicit operation so they cannot be added into the dispatch

; d. if the dispatch line swapped the arguments to get
; every call to put would have its first two arguments swapped
; eg: from (put 'deriv 'op) to (put 'op 'deriv )

