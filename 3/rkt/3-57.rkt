#lang sicp

(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))

; only n-2 additions are required to compute the nth fibonacci number
; this would be exponentially higher if delay didn't memoize because every call
; to (stream-cdr fibs) would have to individually compute all previous fibonacci
; numbers causing it to be exponential
