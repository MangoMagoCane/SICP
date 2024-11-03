#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-39

(define x 10)
(define s (make-serializer))
(parrallel-execute
  (lambda () (set! x ((s (lambda () * (x x))))))
  (s (lambda() (set! x (+ x 1)))))

; 101: P1 sets x to 100 and then P2 increments x to 101
; 121: P2 increments x to 11 and then P1 sets x to x * x
; 11: P2 accesses x, then P1 sets x to 100, then P2 sets x
; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-40

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; 100: P1 accesses x (twice), then P2 sets x to 1000, then P1 sets x 
; 1000: P1 sets x to 100 and then P2 sets x
; 10000: P2 changes x from 10 to 1000 between the two times that 
;        P1 accesses the value of x during the evaluation of (* x x) (* 10 1000)
; 100000: P1 changes x from 10 to 100 between the first two times that 
;         P2 accesses the value of x during the evaluation of (* x x x) (* 10 100 100)
; 1000000: P2 sets x to 1000 and then P1 sets x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-41

; serializing access to the bank balance is not necessary
; because it does not change the state of the account
; if a withdraw and balance read occur in parallel and the read
; executes first, it is still a valid reading

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-42

; this is a safe change to make because the lookups for the
; object's methods and application must be serialized
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-44

; there is no problem here, the inherent difference is that
; in the exchange problem, interleaving regarding intermediate values 
; could occur between it's calls to serialized methods. in the transfer
; problem, interleaving between its method calls are irrelevent except in 
; the case of insufficient funds (which are assumed to not be the case)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
; 3-44

; Louis's reasoning is incorrect, since exchange is serialized
; to two accounts, the subsequent serialized calls to withdraw/deposit
; from exchange to the accounts will not process until exchange has completed 
; causing an infinite loops since the values exchange depends on for completion
; will only complete once complete has
