; #lang sicp
#lang racket/base
(require racket/trace)

; did not solve by myself, spend more time with to understand
(define (fib-log n)
  (define (fib-iter a b p q count)
    
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* q q) (* p p)) ; compute p
                     (+ (* 2 p q) (* q q)) ; compute q
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (trace fib-iter)
  (fib-iter 1 0 0 1 n))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))


(fib-log 12)
(fib 12)