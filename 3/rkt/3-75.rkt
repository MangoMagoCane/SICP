#lang sicp

(define (make-zero-crossings input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream)
                    last-value)
                 2)))
    (cons-stream
      (sign-change-detector avpt last-avpt)
      (make-zero-crossings
        (stream-cdr input-stream) 
        (stream-car input-stream) 
        avpt))))

; Louis Reasoner's issue was using the previous avpt value to
; calculate the new avpt value instead of the stream's previous value

