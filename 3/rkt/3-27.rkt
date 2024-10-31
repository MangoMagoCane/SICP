#lang sicp

; memo-fib computes the nth fibonacci number in O(n) because
; all the recursive (memo-fib (- n 1)) calls occur first populating
; the table which are looked up upon the (memo-fib (- n 2)) calls

; (define memo-fib (memoize fib)) will not work as intended
; if (memo-fib n) is already memoized the time complexity will be 
; O(1), otherwise the recursive calls will not be to memo-fib 
; and thus O(n^2)
