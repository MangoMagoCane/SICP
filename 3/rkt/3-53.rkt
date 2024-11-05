#lang sicp

(define s (cons-stream 1 (add-streams s s)))

; this stream will contain the powers of 2 starting from 1
