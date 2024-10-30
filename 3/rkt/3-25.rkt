#lang sicp

(define (log . messages) 
  (define (inner messages)
    (if (null? messages)
        (newline)
        (begin (display (car messages)) (display " ")
               (inner (cdr messages)))))
  (inner messages))

(define (make-table)
  (list '*table*))

(define (lookup keys table)
  (define (inner keys records)
    (if (null? keys)
        records
        (let ((record (assoc (car keys) records)))
          (if record
              (inner (cdr keys) (cdr record))
              #f))))
  (if (null? keys)
      (error "No keys: LOOKUP" keys)
      (inner keys (cdr table))))

(define (insert! keys value table)
  (define (inner keys table)
    (let ((key (car keys))
          (rest-of-keys (cdr keys))
          (records (cdr table)))
      (let 
        ((record (if (pair? records)
                 (assoc key records)
                 (begin (log "record:" records "was replaced by the nested table:" key)
                        (set-cdr! table (list))
                        #f))))
        (if record
            (if (null? rest-of-keys)
                (set-cdr! record value)
                (inner rest-of-keys record))
            (if (null? rest-of-keys)
                (set-cdr! table 
                          (cons (cons key value)
                                records))
                (begin
                  (set-cdr! table
                            (cons (list key)
                                  records))
                  (inner rest-of-keys (cadr table))))))))
  (if (null? keys)
      (error "No keys: INSERT!" keys)
      (inner keys table))
  'ok)

(define tbl (make-table))
(insert! '(foo bar dog) 'doggy tbl)
(lookup '(foo bar dog) tbl)

(insert! '(foo bar dog hound) 'woof tbl)
(insert! '(foo bar cat) 'kitty tbl)
(insert! '(foo bar bird) 'parrot tbl)

(lookup '(foo bar dog hound) tbl)
(lookup '(foo bar bird) tbl)
(lookup '(foo bar cat) tbl)
