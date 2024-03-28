#lang sicp

(define (average x y)
  (/ (+ x y) 2))
     
(define (calc-roots guess prev-guess improve x)
  (define (good-enough? guess prev-guess)
    (< (abs (/ (- guess prev-guess) guess)) 0.000000001))
  (define (iter guess prev-guess)
    (if (good-enough? guess prev-guess)
        guess
        (iter (improve guess x) guess)))
  (iter guess prev-guess))
  
(define (my_cbrt x)
  (define (imprv-cbrt guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))    
  (calc-roots 1.0 0.0 imprv-cbrt x))

(define (my_sqrt x)
  (define (imprv-sqrt guess x)
    (average guess (/ x guess)))
  (calc-roots 1.0 0.0 imprv-sqrt x))

(my_sqrt 25)
(my_cbrt 125)