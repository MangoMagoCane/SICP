#lang sicp

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    ((cond ((< id1 id2)
             (serializer2 (serializer1 exchange)))
           ((< id2 id1)
             (serializer1 (serializer2 exchange)))
           (else (error "Attempt to exchange with the same account: SERIALIZED-EXCHANGE" 
                        account1)))
      account1
      account2)))

; this deadlock-avoidance method is successful because orginally
; in the case of (serialized-exchange a1 a2) and (serialized-exchange a2 a1)
; running in parallel, the first process would enter a2's protection while
; the second process enters a1's protection causing them block one another
; when trying to enter "account1"'s protection. Always serealizing the account with
; the lowest id would cause both processes to attempt to enter the same account's protection
; blocking one of them and in turn the deadlock
