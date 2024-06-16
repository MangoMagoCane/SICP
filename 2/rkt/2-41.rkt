#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
        (cond ((null? sequence) nil)
              ((predicate (car sequence))
               (cons (car sequence)
                     (filter predicate (cdr sequence))))
              (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap 
    (lambda (i) 
      (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (unique-triple n)
  (flatmap 
    (lambda (i) 
      (map (lambda (j) (cons j i))
           (enumerate-interval (+ (car i) 1) n)))
    (unique-pairs n)))

(define (triple-sum-< n s)
  (filter (lambda (triple) (= s (+ (car triple) (cadr triple) (caddr triple))))
          (unique-triple n)))

(unique-triple 6)
(triple-sum-< 6 8)