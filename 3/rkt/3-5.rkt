#lang sicp

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (integral-test)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (* (monte-carlo trials integral-test)
     (* (- x2 x1) (- y2 y1) 1.0)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
          ((experiment)
            (iter (- trials-remaining 1)
                  (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

(estimate-integral 
  (lambda (x y) (<= (+ (* x x) (* y y)) 1))
  -1.0 1.0 -1.0 1.0 10000000)

