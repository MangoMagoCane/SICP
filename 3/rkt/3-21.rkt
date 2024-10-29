#lang sicp

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (make-queue) (cons '() '()))
(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair))
          (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)))
    queue))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
          (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(delete-queue! q1)
(print-queue q1)

; ben's example produces the output it does because when a queue is printed
; it follow the list of pairs from front-ptr which share a pointer with the
; rear-ptr. Also, the value of the rear-ptr doesn't inherently represent an 
; element in the queue, only which pairs' cdr to set to an inserted element

