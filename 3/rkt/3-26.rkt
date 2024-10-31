#lang sicp

(define key-greater? string>?)
(define (to-string val)
  (cond ((string? val) val) 
        ((number? val) (number->string val))
        ((symbol? val) (symbol->string val))
        (else "Unsupported type: TO-STRING" val)))

(define (make-table)
  (list '*table*))

(define (make-node key left right val)
  (cons key (cons left (cons right val))))
(define node-key car)
(define node-left cadr)
(define node-right caddr)
(define node-val cdddr)

(define (node-find node val)
  (if (null? node)
      #f
      (let ((key (node-key node)))
        (cond ((string=? key val) (node-val node))
              ((string>? key val) (node-find (node-left node) val))
              (else (node-find (node-right node) val))))))

(define (insert! keys value table)
  (define (inner keys nodes)
    (let ((key (car keys))
          (rest-of-keys (cdr keys))
          (sub-table (node-find nodes)))
      (if sub-table
          ()
      
  (if (null? keys)
      (error "No keys: INSERT!" keys)
      (inner (map to-string keys) (cdr table)))
  'ok)
