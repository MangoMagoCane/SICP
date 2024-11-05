#lang sicp

; the two versions would not differ in efficiency if our
; implementation of delay didn't memoize because the calls
; to (sqrt-stream x) and guesses would be equivalent as
; the streams would have to be reconstructed
