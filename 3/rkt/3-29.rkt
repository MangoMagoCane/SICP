#lang sicp

(define (or-gate a b output)
  (let ((not-a (make-wire)) (make-b (make-wire)) (and-out (make-wire)))
    (inverter a not-a)
    (inverter b not-b)
    (and-gate not-a not-b and-out)
    (inverter and-out output)
  'ok))
