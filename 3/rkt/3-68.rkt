#lang sicp

(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (pairs (stream-cdr s) (stream-cdr t))))

; this version of interleave will call itself recursively 
; infinitelly because (pairs (stream-cdr s) (stream-cdr t))
; will be evaluated before being passed to interleave which will 
; in turn call pairs recursively. In stream programming, the base case
; is a cons-stream where the car is terminating and where the cdr may not be 

