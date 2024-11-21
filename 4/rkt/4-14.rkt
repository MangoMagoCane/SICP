#lang sicp

; the problem with Louis' map is (map + ls1 ls2) will
; expand out to (map '(application + env) ls1 ls2) which
; the underlying scheme will not be able to interpret properly.
; Since the meta-circular evaluator is designed to work with such
; expressions, Eva's definition within the evaluator will know how
; to properly interpret.
