; #lang sicp
#lang racket/base
(require racket/trace)

(define (* a b)
  ; (trace *)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double n) (+ n n))
(define (halve n) (/ n 2))

(define (log-mult a b) 
  (trace log-mult)
  (cond ((= b 1) a)
        ((even? b) (double (log-mult a (halve b))))
        (else (+ a (log-mult a (- b 1))))))

(define (log-mult2 a b) 
  (trace log-mult2)
  (cond ((= b 1) a)
        ((even? b) (log-mult2 (double a) (halve b)))
        (else (+ a (log-mult2 a (- b 1))))))

(log-mult 17 19)
(log-mult2 17 19)