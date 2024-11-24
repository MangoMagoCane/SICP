#lang sicp

(define (scan-out-defines-simultaneous body)
  (scan-out-defines
    body
    (lambda (defns rest-of-body)
      (define (eq-rec? exp vars)
        (if (pair? exp)
            (or (eq-rec? (car exp) vars)
                (eq-rec? (cdr exp) vars))
            (memq exp vars)))
      (define (inner clauses lst prev-length)
        (if (null? clauses)
            lst
            (let* ((definitions (map car clauses))
                  (filtered-clauses
                    (filter-both
                      (lambda (clause)
                        (let ((res (eq-rec? (cadr clause) definitions)))
                          (or (and (lambda? (cadr clause))
                                   (eq? (car clause) (if (pair? res) (car res) res)))
                              (not res))))
                      clauses))
                  (available-clauses (car filtered-clauses))
                  (dependent-clauses (cdr filtered-clauses))
                  (curr-length (length dependent-clauses)))
              (if (= curr-length prev-length)
                  (error "Recursively defined internal definitions" clauses)
                  (inner dependent-clauses 
                        (append lst (map (lambda (clause) (cons 'define clause))
                                         available-clauses))
                        curr-length)))))
      (append (inner defns '() -1)
              rest-of-body))))

