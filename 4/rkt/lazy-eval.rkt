#lang sicp

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (filter-both-pos filter-pair) (car filter-pair))
(define (filter-both-neg filter-pair) (cdr filter-pair))
(define (filter-both predicate sequence)
  (define (inner seq t-seq f-seq)
    (cond ((null? seq)
            (cons (reverse t-seq)
                  (reverse f-seq)))
          ((predicate (car seq))
            (inner (cdr seq)
                   (cons (car seq) t-seq)
                   f-seq))
          (else
            (inner (cdr seq)
                   t-seq
                   (cons (car seq) f-seq)))))
  (inner sequence '() '()))

(define (print . exps)
  (for-each (lambda (exp) (display exp) (display " "))
            exps)
  (newline))

(define (eval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp env))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((and? exp) (eval-and exp env))
    ((or? exp) (eval-or exp env))
    ((lambda? exp) (make-procedure (lambda-parameters exp)
                                    (lambda-body exp)
                                    env))
    ((begin? exp)
      (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))
    ((let? exp) (eval (let->combination exp) env))
    ((let*? exp) (eval (let*->combination exp) env))
    ((for? exp) (eval (for->combination exp) env))
    ((application? exp)
      (meta-apply (actual-value (operator exp) env)
                  (operands exp)
                  env))
    (else
      (error "Unknown expression type: EVAL" exp))))

(define (meta-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure
            procedure
            (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              (process-var-args
                (list-of-delayed-args arguments env)
                (procedure-parameters-var-args procedure))
              (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (actual-value exp env)
  (force-it (eval exp env)))
(define (force-it obj)
  (cond ((thunk? obj)
          (let ((result (actual-value (thunk-exp obj)
                                      (thunk-env obj))))
            (set-car! obj 'evaluated-thunk)
            (set-car! (cdr obj)
                      result) ; replace exp with its value
            (set-cdr! (cdr obj)
                      '()) ; forget unneeded env
            result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (variable-args? exp) (tagged-list? exp ':))
(define (process-var-args args params)
  (if (null? args)
      '()
      (let ((new-params
              (if (null? params)
                  '()
                  (rest-exps params))))
        (if (variable-args? params)
            (list (cons (first-exp args) (process-var-args (rest-exps args) new-params)))
            (cons (first-exp args) (process-var-args (rest-exps args) new-params))))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
      (list-of-delayed-args (rest-operands exps)
                            env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (if (last-exp? exps)
      (eval (first-exp exps) env)
      (begin
        (actual-value (first-exp exps) env)
        (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp env)
  (define (rec-quote exp)
    (if (pair? exp)
        (list 'cons (rec-quote (car exp)) (rec-quote (cdr exp)))
        (list 'quote exp)))
  (if (pair? (cadr exp))
      (eval (rec-quote (cadr exp)) env)
      (cadr exp)))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (make-definition definition body)
  (list 'define definition body))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

(define (defn-clause-var clause) (car clause))
(define (defn-clause-exp clause) (cadr clause))

(define (scan-out-defines body proc)
  (define (scan exps defns)
    (let ((curr-exp (first-exp exps)))
      (if (definition? curr-exp)
          (scan
            (rest-exps exps)
            (cons (list (definition-variable curr-exp)
                        (definition-value curr-exp))
                  defns))
          (proc defns exps))))
  (if (definition? (first-exp body))
      (scan body '())
      body))
    
(define (scan-out-defines-let-set body)
  (scan-out-defines
    body
    (lambda (defns rest-of-body)
      (let ((defns-correct-order (reverse defns)))
        (list
          (make-let
            (map
                (lambda (exp)
                  (list (defn-clause-var exp)
                        ''*unassigned*))
                defns-correct-order)
            (append
              (map
                (lambda (exp)
                  (list 'set!
                        (defn-clause-var exp)
                        (defn-clause-exp exp)))
                defns-correct-order)
              rest-of-body)))))))

(define (scan-out-defines-simultaneous body)
  (define (eq-rec? exp vars)
    (if (pair? exp)
        (or (eq-rec? (car exp) vars)
            (eq-rec? (cdr exp) vars))
        (memq exp vars)))
  (define (inner clauses ordered-clauses prev-length)
    #| (print "CLAUSESS:" clauses) |#
    (if (null? clauses)
        ordered-clauses
        (let* ((definitions (map defn-clause-var clauses))
              (filtered-clauses
                (filter-both
                  (lambda (clause)
                    (let ((res (eq-rec? (defn-clause-exp clause) definitions)))
                      (or (and (lambda? (defn-clause-exp clause))
                               (eq? (defn-clause-var clause) (if (pair? res) (car res) res)))
                          (not res))))
                  clauses))
              (available-clauses (filter-both-pos filtered-clauses))
              (dependent-clauses (filter-both-neg filtered-clauses))
              (curr-length (length dependent-clauses)))
          (if (= curr-length prev-length)
              (error "Recursively defined internal definitions" clauses)
              (inner dependent-clauses 
                    (append ordered-clauses (map (lambda (clause) (cons 'define clause))
                                      available-clauses))
                    curr-length)))))
  (scan-out-defines
    body
    (lambda (defns rest-of-body)
      (append (inner defns '() -1)
              rest-of-body))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (make-application procedure parameters)
  (cons procedure parameters))
(define (cond-arrow-action clause) (caddr clause))
(define (cond-arrow-clause? clause)
  (tagged-list? (cond-actions clause) '=>))

(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "else clause isn't last: COND->IF"
                        clauses))
              (if (cond-arrow-clause? first)
                  (make-application
                    (make-lambda '(*reserved-cond*) ; investigate later, may be incorrect
                                 (make-if '*reserved-cond*
                                          (make-application (cond-arrow-action first)
                                                            '(*reserved-cond*))
                                          (expand-clauses rest)))
                    (cond-predicate first))
                  (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest)))))))
  (expand-clauses (cond-clauses exp)))

(define (make-let clauses body)
  (cons 'let (cons clauses body)))
(define (make-named-let var bindings body)
  (cons 'let (cons var bindings body)))
(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (not (pair? (named-let-var exp))))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) 
  (cddr exp)) 
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))
(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cadddr exp))

(define (let->combination exp)
  (define (expand-clauses clauses vars exps)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-application
            (make-lambda (reverse (cons (let-var first) vars))
                         (let-body exp))
            (reverse (cons (let-exp first) exps)))
          (expand-clauses rest
                          (cons (let-var first) vars)
                          (cons (let-exp first) exps)))))
  (define (expand-named-let exp)
    (let ((var (named-let-var exp))
          (bindings (named-let-bindings exp)))
      (make-application
        (make-lambda
          '()
          (list
            (make-definition
              (cons var (map let-var bindings))
              (named-let-body exp))
            (cons var (map let-exp bindings))))
          '())))
  (if (named-let? exp)
      (expand-named-let exp)
      (expand-clauses (let-clauses exp) '() '())))

(define (make-let* clauses body)
  (list 'let* clauses body))
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-clauses exp) (cadr exp))
(define (let*-body exp) (caddr exp))

(define (let*->combination exp)
  (define (expand-clauses clauses)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-let (list first) (let*-body exp))
          (make-let (list first) (expand-clauses rest)))))
  (expand-clauses (let*-clauses exp)))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exps env)
  (define (inner exps)
    (let ((curr-eval (eval (first-exp exps) env)))
      (if curr-eval
          (if (last-exp? exps)
              curr-eval
              (eval-and (rest-exps exps) env))
          'false)))
  (inner (rest-exps exps)))

(define (eval-or exps env)
  (define (inner exps)
    (let ((curr-eval (eval (first-exp exps) env)))
      (if curr-eval
          curr-eval
          (if (last-exp? exps)
              'false
              (eval-or (rest-exps exps) env)))))
  (inner (rest-exps exps)))

(define (for? exp) (tagged-list? exp 'for))
(define (for-body exp) (cddddr exp))
(define (for-initialize exp) (cadr exp))
(define (for-predicate exp) (caddr exp))
(define (for-increment exp) (cadddr exp))

(define (for->combination exp)
  (make-named-let
    '*reserved-for*
    (list (for-initialize exp))
      (make-if
        (for-predicate exp)
        (make-begin
          (append
            (for-body exp)
            (list
              (make-application '*reserved-for*
                                (list (for-increment exp))))))
        'false)))

(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
  #| (list 'procedure parameters (scan-out-defines-let-set body) env)) |#
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters-var-args p) (cadr p))
(define (procedure-parameters p)
  (filter (lambda (arg) (not (eq? arg ':)))
          (cadr p)))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (let ((len-vars (length vars))
        (len-vals (length vals)))
    (if (= len-vars len-vals)
        (cons (make-frame vars vals) base-env)
        (if (< len-vars len-vals)
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied" vars vals)))))

(define (lookup-variable-value var env)
  (define invalid-symbols '(*unassigned*))
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
          (frame-values frame)))))
  (let ((invalid-symbol? (memq var invalid-symbols)))
    (if invalid-symbol?
        (error "Attempted lookup of symbol" (car invalid-symbol?))
        (env-loop env))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
              (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
          (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
              (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list #| (list 'car car) |#
        #| (list 'cdr cdr) |#
        #| (list 'caaar  caaar)  (list 'caadr  caadr)  (list 'cadar  cadar)  (list 'caddr  caddr) |#
        #| (list 'cdaar  cdaar)  (list 'cdadr  cdadr)  (list 'cddar  cddar)  (list 'cdddr  cdddr) |#
        #| (list 'caaaar caaaar) (list 'caaadr caaadr) (list 'caadar caadar) (list 'caaddr caaddr) |#
        #| (list 'cadaar cadaar) (list 'cadadr cadadr) (list 'caddar caddar) (list 'cadddr cadddr) |#
        #| (list 'cdaaar cdaaar) (list 'cdaadr cdaadr) (list 'cdadar cdadar) (list 'cdaddr cdaddr) |#
        #| (list 'cddaar cddaar) (list 'cddadr cddadr) (list 'cdddar cdddar) (list 'cddddr cddddr) |#
        #| (list 'cons cons) |#
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'list list)
        (list 'null? null?)
        (list 'number? number?)
        (list 'string? string?)
        (list 'pair? pair?)
        (list 'symbol? symbol?)
        (list 'eq? eq?)
        (list 'length length)
        (list 'error error)
        (list 'eval eval)
        (list 'apply meta-apply)
        (list 'read read)
        (list '= =)
        (list '< <)
        (list '> >)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list 'newline newline)
        (list 'print (lambda (first . rest)
                       (if (null? rest)
                           (print "PRINT:" first)
                           (apply print (append (list "PRINT:" first) rest)))))
        (list '#t #t)
        (list '#f #f)
        (list '() '())
        ; (list ')
        ; ⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply
    (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (print string))
(define (announce-output string)
  (newline) (print string))
(define (user-print object)
  (if (compound-procedure? object)
      (display (append
                 (list 'compound-procedure
                       (procedure-parameters-var-args object))
                 (append (procedure-body object)
                         '(<procedure-env>))))
      (display object)))
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let* ((input (read))
         (output (actual-value input the-global-environment)))
    (announce-output output-prompt)
    (user-print output))
  (driver-loop))

(define defn-procs 
'(
  #| (define (map proc : lists) |# ; var args broken rn kthxbye
  #|   (define (map-1 proc lst) |#
  #|     (if (null? lst) |#
  #|         '() |#
  #|         (cons (proc (car lst)) |#
  #|               (map-1 proc (cdr lst))))) |#
  #|   (if (null? (car lists)) |#
  #|       '() |#
  #|       (cons |#
  #|         (apply proc (map-1 car lists)) |#
  #|         (apply map |#
  #|               (cons proc (map-1 cdr lists)))))) |#
  (define (map proc items)
    (if (null? items)
        '()
        (cons (proc (car items)) (map proc (cdr items)))))
  (define (cons x y) (lambda (m) (m x y)))
  (define (car z) (z (lambda (p q) p)))
  (define (cdr z) (z (lambda (p q) q)))
  #| (define (list-ref items n) |#
  #|   (if (= n 0) |#
  #|       (car items) |#
  #|       (list-ref (cdr items) (- n 1)))) |#
  #| (define (scale-list items factor) |#
  #|   (map (lambda (x) (* x factor)) items)) |#
  #| (define (add-lists list1 list2) |#
  #|   (cond ((null? list1) list2) |#
  #|         ((null? list2) list1) |#
  #|         (else (cons (+ (car list1) (car list2)) |#
  #|                     (add-lists (cdr list1) (cdr list2)))))) |#
  #| (define ones (cons 1 ones)) |#
  #| (define integers (cons 1 (add-lists ones integers))) |#
  #| (define (integral integrand initial-value dt) |#
  #| (define int |#
  #|   (cons initial-value |#
  #|         (add-lists (scale-list integrand dt) int))) |#
  #|                     int) |#
  #| (define (solve f y0 dt) |#
  #|   (define y (integral dy y0 dt)) |#
  #|   (define dy (map f y)) |#
  #|   y) |#
))
(for-each (lambda (exp) (actual-value exp the-global-environment))
          defn-procs)
(print "----------------------------------------------")

(driver-loop)

