#lang sicp

(define rand 
  (let ((x random-init))
    (define (dispatch sym)
      (cond ((eq? sym 'generate) 
              (set! x (rand-update x))
              x)
            ((eq? sym 'reset) 
              (lambda (new-val) (set! x new-val)))
            (else (error "Invalid symbol: RAND"
                         sym))))
    dispatch))
