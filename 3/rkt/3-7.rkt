#lang sicp

#| (define (make-account balance . passwords) |#
#|   (define (withdraw amount) |#
#|     (cond ((>= balance amount) |#
#|             (set! balance (- balance amount)) |#
#|             balance) |#
#|         (else "Insufficient funds"))) |#
#|   (define (deposit amount) |#
#|     (set! balance (+ balance amount)) |#
#|     balance) |#
#|   (define (add-password password) |#
#|     (set! passwords (cons password passwords)) |#
#|     dispatch) |#
#|   (define (dispatch submitted-password message) |#
#|     (if (memq submitted-password passwords) |#
#|         (cond ((eq? message 'withdraw)  withdraw) |#
#|               ((eq? message 'deposit) deposit) |#
#|               ((eq? message 'add-password) add-password) |#
#|               (else (error "Unknown request: MAKE-ACCOUNT" message))) |#
#|         (lambda (x) "Incorrect password"))) |#
#|   dispatch) |#
#||#
#| (define (make-joint account password new-password) |#
#|   ((account password 'add-password) new-password)) |#
(define (invalid x) "Invalid password")

(define (make-account balance password)
  (define (withdraw amount)
    (cond ((>= balance amount)
            (set! balance (- balance amount))
            balance)
        (else "Insufficient funds")))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch submitted-password message)
    (if (eq? submitted-password password)
        (cond ((eq? message 'withdraw) withdraw)
              ((eq? message 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" message)))
        invalid))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch submitted-password message)
    (if (eq? submitted-password new-password)
        (account password message)
        invalid))
  dispatch)

(define foo (make-account 100 'f))
((foo 'f 'withdraw) 40)
((foo 'b 'deposit) 50)
(define bar (make-joint foo 'f 'b))
((bar 'b 'withdraw) 40)
((bar 'b 'deposit) 10)
((bar 'f 'deposit) 10)
((foo 'b 'deposit) 10)

