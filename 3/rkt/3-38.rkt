#lang sicp

; originally 100
(set! balance (+ balance 10)) ; 1
(set! balance (- balance 20)) ; 2
(set! balance (- balance (/ balance 2))) ; 3

; 1-2-3 45
; 1-3-2 35
; 2-1-3 45
; 2-3-1 50
; 3-1-2 40
; 3-2-1 40

; (1-2)-3 40
; (3-2)-1 90
; (3-2-1) 110
