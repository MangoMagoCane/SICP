#lang sicp

(define z (make-complex-from-real-img 3 4)) ; '(complex rectangular 3 4)
(magnitude z)

(apply-generic 'magnitude '(complex rectangular 3 4))
(apply (get 'magnitude '(complex)) (map cdr '((complex rectangular 3 4))))
(apply (get 'magnitude '(complex)) '((rectangular 3 4)))
(apply magnitude '((rectangular 3 4)))
(magnitude '(rectangular 3 4))

(apply-generic 'magnitude '(rectangular 3 4))
(apply (get 'magnitude '(rectangular)) (map cdr '((rectangular 3 4))))
(apply (get 'magnitude '(rectangular)) '((3 4)))
(apply (lambda (z) (...)) '((3 4)))
((lambda (z) (...)) '(3 4))
5

; this works because the first time magnitude is looked up
; with apply-generic it returns and immediately invokes the
; same magnitude function but with the 'complex tag stripped off
; which finds and immediately invokes the procedure internal to
; rectangular package
