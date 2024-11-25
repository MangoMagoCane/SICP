#lang sicp

(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10)))
count ; 1
; x in the enclosing call to id is the
; result of evaluating the thunk (id 10)
w ; 10
count ; 2

