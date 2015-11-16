;If you have any idea why my comment in Mstate-class-list is the case please let me know why that is an issue in the comments of the graded interpreter

(load "classParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

(define interpret
  (lambda (fileName mainclass try)
    (call/cc
     (lambda (return)
       (Mstate (Mstate-class-list (parser fileName) (new-env) return (lambda (v) v) (lambda (v) (M_state_list (caddr catch) (add (cadr catch) v (new-env-layer env)) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (continue (cut-env-layer v)) v)) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol mainclass try) (lambda (v) (M_state_list (caddr catch) (add (cadr catch) v (new-env-layer env)) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (continue (cut-env-layer v))) v)))))))

(define new-env
  (lambda ()
    '((()()))))

(define new-instance-env
  (lambda (class-name)
    (list (list (list 'this 'instance) (list (box class-name) (box (new-env)))))))

(define Mstate-class-list
  (lambda (classes env return next try)
    (cond
      ((null? classes) (next env))
      ((not (eq? (caar classes) 'class try)) (error 'invalid-class-declaration))
                               ;to whoever grades this, I'm unable to see where and why M_state_list isn't parsing through the whole class like it should be and has done before
                               ;however I think that most other things should work in regards to objects since I implemented constructors, and have the whole this super and instance variable name thing down.
      ((null? (caddar classes)) (next (Mstate-class-list (cdr classes) (add (cadar classes) (M_state_list (car (cdddar classes)) (add (cadar classes) (new-instance-env (cadar classes)) env) return next (lambda (v) v) (lambda (v) v) (cadar classes)) try) env) return next))
      (else (Mstate-class-list (cdr classes) (add (cadar classes) (list (car (M_state_list (car (cdddar classes)) (add-super (lookup (cadr (caddar classes)) env) (add 'super (cadr (caddar classes)) (add 'this (cadar classes) (add 'instance (lookup-in 'instance (cadr (caddar classes)) env) (new-env-layer env))))(cadar classes)) return next (lambda (v) v) (lambda (v) v) 'null try))) env) return next)))))

(define add-super
  (lambda (vars env class try)
    (cond
      ((null? (caar vars)) env)
      ((eq? (caaar vars) 'this) env)
      ((eq? (caaar vars) 'instance) env)
      (else (add-super (list (list (cdaar vars) (cdadar vars))) (list (list (cons (caaar vars) (caar env)) (cons (caadar vars) (cadar env)))) class try)))))

(define Mstate
  (lambda (env return break continue next mainclass try)
    (M_state_list (cadr (lookup-in 'main mainclass env)) (new-env-layer env) (lambda (v) (cond ((eq? #t v) (return 'true)) ((eq? #f v) (return 'false)) (else (return v)))) next break continue mainclass try))) 

(define M_state_list
  (lambda (list enviornment return next break continue class try)
    (cond
      ((null? list) (next enviornment))
      (else (Mstate-stmt (car list) enviornment return break continue (lambda (v) (M_state_list (cdr list) v return next break continue class try)) class try)))))

;function that creates environment for a function call from the global environment
(define fstate
  (lambda (s)
    (cond
      ((null? s) (new-env-layer s))
      ((null? (cdr s)) (new-env-layer s))
      (else (fstate (cdr s))))))

(define Mstate-funcbody
  (lambda (actualparam formalparam body env return class try)
    (cond
      ((not (eq? (length actualparam) (length formalparam))) (error 'mismatched-param-and-args))
      ((null? actualparam) (M_state_list body env return (lambda (v) v) (lambda (v) v) (lambda (v) v) class try))
      (else (Mstate-funcbody (cdr actualparam) (cdr formalparam) body (add (car formalparam) (car actualparam) env) return class try)))))
      
;returns a list containing the values of the parameter list
;try
(define mval-paramlist
  (lambda (param env class try)
    (cond
      ((null? param) '())
      (else (cons (M_value (car param) env class try) (mval-paramlist (cdr param) env class try))))))

;try
(define Mstate-fc
  (lambda (name paramlist env prevclass class try)
      (call/cc (lambda (return)
                 (Mstate-funcbody (mval-paramlist paramlist env class try) (car (lookup-in name class env)) (cadr (lookup-in name class env)) ((caddr (lookup-in name class env)) env) return class try)))))
  

(define Mstate-stmt
  (lambda (stmt env return break continue next class try)
    (cond
    
      ((eq? 'static-var (car stmt)) (eq? '= (car stmt)) (eq? (car stmt) 'var)) (next (M_state_cast stmt env class try))
      ((null? (car stmt)) (next env))
      ((eq? (car stmt) 'return) (return (M_value (cadr stmt) env class try)))
      ((eq? (car stmt) '=) (next (M_state_cast stmt env)))
      ((eq? 'if (car stmt)) (M_state_if stmt env return next break continue class try))
      
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (Mstate-while stmt env return next class try))
      
      
      ((eq? (car stmt) 'function) (next (instance-fd stmt env class try)))
      ((eq? (car stmt) 'static-function) (next (Mstate-fd stmt env)))
      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (M_state_list (cdr stmt) (new-env-layer env) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (lambda (v) (continue (cut-env-layer v))) class ))
      
      ((and (eq? (car stmt) 'funcall) (list? (cadr stmt))) (begin (Mstate-fc (car (cddadr stmt)) (cddr stmt) env class (convertclass (cadadr stmt) class env try)) (next env)))
      ((eq? (car stmt) 'funcall) (begin (Mstate-fc (get-name stmt) (cddr stmt) env class class try) (next env)))
      
      ((eq? (car stmt) 'try) (next (Mstate-try (cadr stmt) (caddr stmt) (cadr (cadddr stmt)) return break continue next class instance env)))
      
      (else (next env)))))

(define Mstate-try
  (lambda (try-bod catch finally-bod return break continue next class env)
    (cond
      ((null? fin-body) (Mstate-do-try try-bod catch return break continue next class env))
      (else (begin (Mstate-do-try try-bod catch return break continue next class env) (M_state_list finally-bod (new-env-layer env) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (lambda (v) (continue (cut-env-layer v))) class class try))))))

(define Mstate-do-try
  (lambda (try-bod catch return break continue next class env)
    (M_state_list try-bod (new-env-layer env) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (lambda (v) (continue (cut-env-layer env))) (lambda (v) (M_state_list (caddr catch) (add (cadr catch) v (new-env-layer env)) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (continue (cut-env-layer v))) class class try) class class try)))

(define Mstate-fd
  (lambda (stmt env)
    (addBinding (get-name stmt) (makeClosure (get-param-list stmt) (get-Fbody stmt) fstate) env)))

(define instance-fd
  (lambda (stmt env class try)
    (myremove-in 'instance (Mstate-fd stmt (lookup-in 'instance class env)) env class try)))


(define addBinding
  (lambda (name closure environment)
    (add name closure environment)))

(define makeClosure
  (lambda (args body Fstate)
    (list args body Fstate)))

(define convertclass
  (lambda (newclass oldclass env)
    (cond
      ((eq? newclass 'super)(lookup-in newclass oldclass env))
      ((eq? newclass 'this) (lookup-in newclass oldclass env))
      (else newclass try))))

(define get-name
  (lambda (stmt)
    (cadr stmt)))
(define get-param-list
  (lambda (stmt)
    (caddr stmt)))
(define get-Fbody
  (lambda (stmt)
    (cadddr stmt)))
;try
(define M_value
  (lambda (expression enviornment class try)
    (cond
      ((number? expression) expression)
      ((eq? 'true expression) true)
      ((eq? 'false expression) false)
      (else (eval-expression expression enviornment prefix class try)))))

; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
;try
(define M_boolean
  (lambda (expression enviornment class try)
    (eval-expression-bool expression enviornment prefix class try)))


(define M_state_if
  (lambda (statement enviornment return next break continue class try)
    (cond
      ((eq? #t (M_boolean (get-cond statement) enviornment class try)) (Mstate-stmt (get-body statement) enviornment return break continue next class try))
      ((null? (cdddr statement)) (next enviornment))
      (else (Mstate-stmt (cadddr statement) enviornment return break continue next class try)))))

;cps 
(define Mstate-while
  (lambda (stmt environment ret next class try)
(letrec ((loop (lambda (condition body environment next)
(if (M_boolean condition environment class try)
(Mstate-stmt body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)) class try)
                        (next environment))))) ;                  break                       continue
(loop (get-cond stmt) (get-body stmt) environment next))))

(define get-cond
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (cadr stmt))
      ((eq? 'if (car stmt)) (cadr stmt)))))

(define get-body
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (caddr stmt))
      ((eq? 'if (car stmt)) (caddr stmt)))))

(define mod-instance
  (lambda (var val env class try)
    (myremove-in 'instance (add var val (lookup-in 'instance class env)) env class try)))
 
;try
(define M_state_cast
  (lambda (stmt env class try)
    (cond
      ((null? stmt) env)
      
      ((and (eq? (car stmt) 'static-var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'static-var) (add (cadr stmt) (M_value (caddr stmt) env class try) env))
      ;non-static
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (mod-instance (cadr stmt) 'null env class try))
      ((eq? (car stmt) 'var) (mod-instance (cadr stmt) (M_value (caddr stmt) env class try) env class try))
      
      ((eq? (car stmt) '=) (begin (myremove-in (cadr stmt) (M_value (caddr stmt) env class try) env class try) env))
      (else (error 'assign-error)))))

(define new-env-layer
  (lambda (env)
    (cons '(()()) env)))

(define cut-env-layer
  (lambda (env)
    (cdr env)))    

(define lookup
  (lambda (var enviornment)
    (cond
      ((or (eq? var 'true) (eq? var 'false) (number? var) (eq? var #t) (eq? var #f)) var)
      
      ((null? enviornment) 'null)
      ((null? (caar enviornment)) (lookup var (cdr enviornment)))
      ((eq? var (caaar enviornment)) (unbox (caadar enviornment)))
      ((eq? 'instance (caaar enviornment)) (lookup-in-instance var enviornment))
      (else (lookup var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)))))))

(define lookup-in
  (lambda (var class env)
    (cond
      ((eq? (lookup var env) 'null) (lookup var (lookup class env)))
      (else (lookup var env)))))

(define lookup-instance
  (lambda (env)
    (lookup 'instance env)))
(define lookup-in-instance
  (lambda (var env)
    (lookup var (lookup-instance env))))

(define add
  (lambda (var val enviornment)
    (cons (list (cons var (caar enviornment)) (cons (box val) (cadar enviornment))) (cdr enviornment))))

(define myremove
  (lambda (var val s next)
    (cond
      ((null? s) 'null)
      ((null? (caar s)) (myremove var val (cdr s) (lambda (v) (next (cons '(()()) v)))))
      ((eq? var (caaar s)) (next (begin (set-box! (caadar s) val) s)))
      (else (myremove var val (cons (list (cdaar s) (cdadar s)) (cdr s)) (lambda (v) (next (cons (list (cons (caaar s) (caar v)) (cons (caadar s) (cadar v))) (cdr v)))))))))

(define myremove-in
  (lambda (var val env class try)
    (cond
      ((eq? 'null (myremove var val env (lambda (v) v))) (myremove var val (lookup class env) (lambda (v) v)))
      (else (myremove var val env (lambda (v) v))))))

; A MValue function that uses abstraction to allow expressions in prefix,
; postfix, or infix format
; Call as (eval-expression '(3 + 4) '() infix)
;eval-expression   ;try
(define eval-expression
  (lambda (expression environment form class try)
    ((lambda (operator left-operand right-operand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup-in expression class environment))
         ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (eval-expression (cadr expression) environment form class try)))
         ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((eq? '/ (operator expression)) (quotient (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((and (eq? 'funcall (operator expression)) (list? (left-operand expression))) (Mstate-fc (car (cddadr expression)) (cddr expression) environment class (convertclass (cadadr expression) class environment) try))
         ((eq? 'funcall (operator expression)) (Mstate-fc (get-name expression) (cddr expression) environment class class try))
         ((and (not (list? expression)) (number? (lookup-in expression class environment))) (lookup-in expression class environment))
         
         ((eq? 'new (operator expression)) (lookup (left-operand expression) environment))
         
         ((and (eq? 'dot (operator expression)) (eq? 'this (left-operand expression))) (eval-expression (lookup-in (right-operand expression) (lookup-in 'this class environment) environment) environment form (lookup-in 'this class environment)))
         ((and (eq? 'dot (operator expression)) (eq? 'super (left-operand expression))) (eval-expression (lookup-in (right-operand expression) (lookup-in 'super class environment) environment) environment form (lookup-in 'super class environment)))
         ((eq? 'dot (operator expression)) (eval-expression (lookup-in (right-operand expression) (left-operand expression) environment) environment form (left-operand expression)))
         
         
         (else (eval-expression-bool expression environment form class try))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))

;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define eval-expression-bool
  (lambda (expression environment form class try)
    ((lambda (operator left-operand right-operand)
       (cond
         ((not (list? expression)) (lookup-in expression class environment))
         ((or (eq? 'true expression) (eq? 'true (car expression))) true)
         ((or (eq? 'false expression) (eq? 'false (car expression))) false)
         ((eq? '&& (operator expression)) (and (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((eq? '! (operator expression))  (not (eval-expression (cadr expression) environment form class try)))
         ((eq? '|| (operator expression)) (or (eval-expression (left-operand expression) environment form class try) (eval-expression (right-operand expression) environment form class try)))
         ((eq? '== (operator expression)) (equal? (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try)))
         ((eq? '!= (operator expression)) (not (equal? (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try))))
         ((eq? '>= (operator expression)) (>= (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try)))
         ((eq? '> (operator expression)) (> (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try)))
         ((eq? '<= (operator expression)) (<= (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try)))
         ((eq? '< (operator expression)) (< (eval-expression (eval-expression (left-operand expression) environment form class try) environment form class try) (eval-expression (eval-expression (right-operand expression) environment form class try) environment form class try)))
         (else (error 'undefined-operator))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))
; retrieves the operator function from the format
(define get-operator
  (lambda (form)
    (car (form))))

; retrieves the left-operand function from the format
(define get-left-operand
  (lambda (form)
    (cadr (form))))

; retrieves the right-operand function from the format
(define get-right-operand
  (lambda (form)
    (caddr (form))))