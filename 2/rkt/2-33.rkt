#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (x y)) nil sequence))
; (define (append seq1 seq2)
;   (accumulate cons ⟨??⟩ ⟨??⟩))
; (define (length sequence)
;   (accumulate ⟨??⟩ 0 sequence))

(map (lambda (x) (* x x)) (list 1 2 3))