#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (key entry) (car entry))

(define (lookup given-key set-of-records)
  (let ((curr-key (key (entry set-of-records))))
    (cond ((null? set-of-records) false)
          ((> given-key curr-key) (lookup given-key (right-branch (set-of-records))))
          ((< given-key curr-key) (lookup given-key (left-branch (set-of-records))))
          (else (entry set-of-records)))))
