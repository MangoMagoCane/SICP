#lang sicp

(define (for-each proc items)
  (define (iter items)
    (cond (not (null? items) 
               (and (proc (car items)) 
                    (iter (cdr items))))))
          
  (iter items))


(for-each (lambda (x)
                  (newline)
                  (display x))
          (list 57 321 88))