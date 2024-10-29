#lang sicp

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque elem)
  (set-car! deque elem))
(define (set-rear-ptr! deque elem)
  (set-cdr! deque elem))

(define (make-deque) (cons '() '()))
(define (empty-deque? deque)
  (null? (front-ptr deque)))

(define (make-elem item) (cons item (cons '() '())))
(define (elem-item elem) (car elem))

(define (elem-pointers elem) (cdr elem))
(define (elem-prev-elem elem) (cadr elem))
(define (elem-next-elem elem) (cddr elem))

(define (set-elem-next-p! elem next-elem) 
  (set-cdr! (elem-pointers elem) next-elem))
(define (set-elem-prev-p! elem prev-elem) 
  (set-car! (elem-pointers elem) prev-elem))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (elem-item (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (elem-item (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-elem (make-elem item)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-elem)
            (set-rear-ptr! deque new-elem))
          (else
            (let ((old-front-ptr (front-ptr deque)))
              (set-elem-next-p! new-elem old-front-ptr)
              (set-elem-prev-p! old-front-ptr new-elem)
              (set-front-ptr! deque new-elem)))))
    deque)

(define (rear-insert-deque! deque item)
  (let ((new-elem (make-elem item)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-elem)
            (set-rear-ptr! deque new-elem))
          (else
            (let ((old-rear-elem (rear-ptr deque)))
              (set-elem-prev-p! new-elem old-rear-elem)
              (set-elem-next-p! old-rear-elem new-elem)
              (set-rear-ptr! deque new-elem)))))
  deque)

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error ("FRONT-DELETE! called with an empty deque" deque)))
        (else 
          (let ((new-front-elem (elem-next-elem (front-ptr deque))))
            (set-front-ptr! deque new-front-elem)
            (set-elem-prev-p! new-front-elem '())
            deque))))

(define (rear-delete-deque! deque)
  (if (empty-deque? deque)
      (error ("REAR-DELETE! called with an empty deque" deque))
      (let ((new-rear-elem (elem-prev-elem (rear-ptr deque))))
        (cond ((null? new-rear-elem)
                (set-front-ptr! deque '())
                (set-rear-ptr! deque '()))
            (else 
              (set-rear-ptr! deque new-rear-elem)
              (set-elem-next-p! new-rear-elem '())))
       deque)))

(define (print-deque deque)
  (define (print-elems elems)
    (display (elem-item elems))
    (let ((next-elem (elem-next-elem elems)))
      (cond ((not (null? next-elem))
              (display " ")
              (print-elems next-elem)))))
  (if (empty-deque? deque)
      (display "()")
      (begin
        (display "(")
        (print-elems (front-ptr deque))
        (display ")")))
  (newline))

(define (put deque)
  (display "---PRINTING STARTS HERE---") (newline)
  (print-deque deque)
  (display "---ENDS---") (newline))

(define dq (make-deque))
(front-insert-deque! dq 3)
(front-insert-deque! dq 2)
(front-insert-deque! dq 1)
(rear-insert-deque! dq 4)
(rear-insert-deque! dq 5)
(put dq)
(front-delete-deque! dq)
(front-delete-deque! dq)
(rear-delete-deque! dq)
(rear-delete-deque! dq)
(put dq)
(front-insert-deque! dq 2)
(front-insert-deque! dq 1)
(put dq)
(rear-delete-deque! dq)
#| (front-delete-deque! dq) |#
(put dq)
#| (front-insert-deque! dq 1) |#
#| (put dq) |#