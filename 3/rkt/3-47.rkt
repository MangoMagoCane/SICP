#lang sicp

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
            (if (test-and-set! cell)
                (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) #t (begin (set-car! cell #t) #f)))

(define (semaphore n)
  (let ((process-count 0)
        (semaphore-lock (make-mutex)))
    (define (acquire)
      (semaphore-lock 'acquire)
      (if (< process-count n)
          (begin (set! process-count (+ process-count 1))
                 (semaphore-lock 'release))
          (begin (semaphore-lock 'release)
                 (acquire))))
    (define (release)
      (semaphore-lock 'acquire)
      (set! process-count (- process-count 1))
      (semaphore-lock 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release))))
    the-semaphore)

