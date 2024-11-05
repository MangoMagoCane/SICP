#lang sicp

(define (integrate-series S)
  (stream-map / S integers))

(define cosine-series (cons-stream 1 (integrate-series (stream-map - sine-stream))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
