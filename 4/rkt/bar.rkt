(define apply-in-underlying-scheme apply)

(define (eval exp env)
  (print "EVAL--" exp)
  (cond ((self-evaluating? exp) (print "self-evaluating") exp)
        ((variable? exp) (print "variable") (lookup-variable-value exp env))
        ((quoted? exp) (print "quoted") (text-of-quotation exp))
        ((assignment? exp) (print "assignment") (eval-assignment exp env))
        ((definition? exp) (print "definition") (eval-definition exp env))
        ((if? exp) (print "if") (eval-if exp env))
        ((and? exp) (print "and") (eval-and exp env))
        ((or? exp) (print "or") (eval-or exp env))
        ((lambda? exp) (print "lambda") (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp) (print "begin")
          (eval-sequence (begin-actions exp) env))
        ((cond? exp) (print "cond") (eval (cond->if exp) env))
        ((let? exp) (print "let") (eval (let->combination exp) env))
        ((let*? exp) (print "let*") (eval (let*->combination exp) env))
        ((application? exp) (print "application")
          (meta-apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (foo args params)
  (if (null? args)
      '()
      (let ((new-params
              (if (null? params)
                  params
                  (cdr params))))
        (if (variable-args? params)
            (list (cons (car args) (foo (cdr args) new-params)))
            (cons (car args) (foo (cdr args) new-params))))))

(define (meta-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
          (eval-sequence
            (procedure-body procedure)
            (extend-environment
              (procedure-parameters procedure)
              (foo arguments (procedure-parameters-var-args procedure))
              (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

(define (variable-args? exp) (tagged-list? exp '$))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
(define (eval-sequence exps env)
  (if (last-exp? exps)
      (eval (first-exp exps) env)
      (begin
        (eval (first-exp exps) env)
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
  (cond ((number? exp) '#t)
        ((string? exp) '#t)
        (else '#f)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      'false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (list (cddr exp))))) ; body
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (caddr exp))
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
                  (error "ELSE clause isn't last: COND->IF"
                        clauses))
              (if (cond-arrow-clause? first)
                  (make-application
                    (make-lambda '(_RESERVED_COND)
                                (make-if '_RESERVED-COND
                                          (make-application (cond-arrow-action first)
                                                            '(_RESERVED-COND))
                                          (expand-clauses rest)))
                    (cond-predicate first))
                  (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          (expand-clauses rest)))))))
  (expand-clauses (cond-clauses exp)))

(define (make-let clauses body)
  (list 'let clauses body))
(define (let? exp) (tagged-list? exp 'let))
(define (let-clauses exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-var clause) (car clause))
(define (let-exp clause) (cadr clause))

(define (let->combination exp)
  (define (expand-clauses clauses vars exps)
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (null? rest)
          (make-application
            (make-lambda (reverse (cons (let-var first) vars))
                         (list (let-body exp)))
            (reverse (cons (let-exp first) exps)))
          (expand-clauses rest 
                          (cons (let-var first) vars) 
                          (cons (let-exp first) exps)))))
  (expand-clauses (let-clauses exp) '() '()))

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

(define (true? x) (not (eq? x 'false)))
(define (false? x) (eq? x 'false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters-var-args p) (cadr p))
(define (procedure-parameters p)
  (filter (lambda (arg) (not (eq? arg '$)))
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
  (env-loop env))

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
  (list (list 'car car)
        (list 'cdr cdr)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'eval eval)
        (list 'display display)
        (list 'newline newline)
        (list 'print print)
        ; ⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt "   --- SUB M-Eval input:")
(define output-prompt "   --- SUB M-Eval value:")
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters-var-args object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (print "INPUT:" input)
    (announce-output output-prompt)
    (let ((output (eval input the-global-environment)))
      (user-print output)))
  (driver-loop))

(define map-proc
  '(define (map proc $ lists)
    (define (map-1 proc lst)
      (if (null? lst)
          '()
          (cons (proc (car lst))
                (map-1 proc (cdr lst)))))
    (if (null? (car lists))
        '()
        (cons
          (apply proc (map-1 car lists))
          (apply map
                 (cons proc (map-1 cdr lists)))))))

(eval map-proc
  the-global-environment)

(driver-loop)

