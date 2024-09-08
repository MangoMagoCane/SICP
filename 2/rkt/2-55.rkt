#lang sicp

(car ''abracadabra)
; This expression evalutes to quote because it expands to
; (car (quote (quote (abracadabra))))
; the innermost quote is not evaluated so what is passed to car is (`quote `abracadabra)
; returning quote