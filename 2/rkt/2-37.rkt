#lang sicp
; couldn't solve alone

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) 
       m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row))
         m)))

(define matrix1 (list (list 1 2 3 4) (list 5 6 7 8) (list 9 10 11 12))) 
(define one-vector (list 1 1 1 1))
(define vector1 (list 1 2 3 4))

; (dot-product vector1 one-vector)
; (matrix-*-vector matrix1 one-vector)
matrix1
(transpose matrix1)