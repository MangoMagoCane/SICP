#lang sicp

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (flip-sign conditional x)
  (if conditional
      (- x)
      x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))
       (n (if (or (and (< n 0) (< d 0)) (< d 0)) (- n) n))
       (d (if (< d 0) (- d) d)))
    (cons (/ n g) (/ d g))))

(define (make-rat2 n d)
  (let ((g ((if (> d 0) + -) (abs (gcd n d)))))
    (cons (/ n g) (/ d g))))

(print-rat (make-rat2 1 4)) ; pos
(print-rat (make-rat2 -1 -4)) ; pos
(print-rat (make-rat2 -1 4)) ; neg
(print-rat (make-rat2 1 -4)) ; neg

; (gcd 1 -4)