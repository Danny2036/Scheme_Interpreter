(load "classParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

(define interpret
  (lambda (fileName classname)
    (call/cc
     (lambda (return)                                                                                                 ;classname or null
       (Mstate (string->symbol classname) (M_state_list (parser fileName) '((()())) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol classname)) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol classname))))))

;class
(define M_state_list
  (lambda (list enviornment return next break continue class)
    (cond
      ((null? list) (next enviornment))
      (else (Mstate-stmt (car list) enviornment return break continue (lambda (v) (M_state_list (cdr list) v return next break continue class)) class)))))

(define Mstate
  (lambda (classname env return break continue next class)
    (M_state_list (cadr (method-lookup classname 'main env)) (make-final-env classname env) return next break continue env))) 

(define make-final-env
  (lambda (classname env)
    (append
     (list (car (get-method-env classname env))
          (car (get-field-env classname env))
          (car (get-instance-env classname env))
          (get-parent classname env))
     env)))

(define method-lookup
  (lambda (classname varname env)
    (lookup varname (get-method-env classname env))))

(define main-env
  (lambda (classname env)
    (lookup classname env)))

(define Mstate-stmt
  (lambda (stmt env return break continue next class)
    (cond
      ((or (eq? 'var (car stmt)) (eq? '= (car stmt))) (next (M_state_cast stmt env class)))
      ((null? (car stmt)) (Mstate-stmt stmt env env break continue next class))
      ((eq? (car stmt) 'return) (return (M_value (cadr stmt) env class)))
      ((eq? (car stmt) '=) (next (M_state_cast stmt env class)))
      ((eq? 'if (car stmt)) (M_state_if stmt env return next break continue class))
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (Mstate-while stmt env return next class))
      ((eq? (car stmt) 'function) (next (Mstate-fd stmt env return next break continue)))
      ((eq? (car stmt) 'funcall) (begin (Mstate-fc (get-name stmt) (cddr stmt) env class) (next env)))
      
      ((eq? (car stmt) 'class) (begin (set! env (add (cadr stmt) (new-class stmt env) env)) (M_state_list (cadddr stmt) env return next break continue (cadr stmt))))
      
      ((eq? (car stmt) 'static-function) (set-method-env class (Mstate-fd stmt (get-method-env class env) return next break continue) env next))
      ;((eq? (car stmt) 'static-function) (next (set-method-env class (box (add (cadr stmt) (caddr stmt) (unbox (get-method-env class env)))) env)))
      ((eq? (car stmt) 'static-var) (set-field-env class (M_state_cast_static stmt (get-field-env class env) class) env next))
      ((eq? (car stmt) 'dot) (dot-eval (cadr stmt) (caddr stmt) env class))

      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (M_state_list (cdr stmt) (new-env-layer env) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (lambda (v) (continue (cut-env-layer v))) class))
      (else (next env)))))

(define dot-eval
  (lambda (class called env Genv)
    (cond
      ((eq? 'super class) (lookup-super called env #f))
      (else (lookup called (make-final-env class env))))))

(define new-class
  (lambda (stmt env)                 ;method   ;field      ;instance
    (cond                ;  (((name) ((()())    (()())     (()())       (parent))
      ((null? (caddr stmt)) (list '((()())) '((()()))  '((()())) '(()())))
      (else (list (get-method-env (get-extends stmt) env)
                  (get-field-env (get-extends stmt) env)
                  (get-instance-env (get-extends stmt) env)
                  (get-parent (get-extends stmt) env))))))
  
(define get-extends
  (lambda (stmt)
    (car (cdaddr stmt))))


(define get-method-env
  (lambda (class-name env)
    (car (lookup class-name env))))
(define set-method-env
  (lambda (class-name new env next)
    (myremove class-name (list new (get-field-env class-name env) (get-instance-env class-name env) (get-parent class-name env)) env next)))
    
(define get-field-env
  (lambda (class-name env)
    (cadr (lookup class-name env))))
(define set-field-env
  (lambda (class-name new env next )
    (myremove class-name (list (get-method-env class-name env) new (get-instance-env class-name env) (get-parent class-name env)) env next)))

(define get-instance-env
  (lambda (class-name env)
    (caddr (lookup class-name env))))
(define set-instance-env
  (lambda (class-name new env next)
    (myremove class-name (list (get-method-env class-name env) (get-field-env class-name env) new (get-parent class-name env)) env next)))
    
(define get-parent
  (lambda (class-name env)
    (cadddr (lookup class-name env))))


(define f-env
  (lambda (s)
    (cond
      ((null? s) (new-env-layer s))
      ((null? (cdr s)) (new-env-layer s))
      (else (f-env (cdr s))))))


(define Mstate-funcbody
  (lambda (actualparam formalparam body env return class)
    (cond
      ((not (eq? (length actualparam) (length formalparam))) (error 'mismatched-param-and-args))
      ((null? actualparam) (M_state_list body env return (lambda (v) v) (lambda (v) v) (lambda (v) v) class))
      (else (Mstate-funcbody (cdr actualparam) (cdr formalparam) body (add (car formalparam) (car actualparam) env) return class)))))
 

(define mval-paramlist
  (lambda (param env class)
    (cond
      ((null? param) '())
      (else (cons (M_value (car param) env class) (mval-paramlist (cdr param) env class))))))


(define Mstate-fc
  (lambda (name paramlist env class)
      (call/cc (lambda (return)
                 (cond
                   ;((list? name) (M_value name paramlist class))
                   (else (Mstate-funcbody (mval-paramlist paramlist env class) (car (to-lookup name env)) (cadr (to-lookup name env)) ((caddr (to-lookup name env)) env) return class)))))))
(define to-lookup
  (lambda (name env)
    (cond
      ((list? name) name)
      (else (lookup name env)))))
(define Mstate-fd
  (lambda (stmt env ret next break continue)
    (addBinding (get-name stmt) (makeClosure (get-param-list stmt) (get-Fbody stmt) f-env) env)))



(define addBinding
  (lambda (name val env)
    (add name val env)))


(define makeClosure
  (lambda (args body Fstate)
    (list args body Fstate)))


(define get-name
  (lambda (stmt)
    (cadr stmt)))
(define get-param-list
  (lambda (stmt)
    (caddr stmt)))
(define get-Fbody
  (lambda (stmt)
    (cadddr stmt)))


(define M_value
  (lambda (stmt env class)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) true)
      ((eq? 'false stmt) false)
      ((not (list? stmt)) (lookup stmt env))
      ((and (eq? (car stmt) 'funcall) (list? (cadr stmt))) (Mstate-fc (caddr (get-name stmt)) (cddr stmt) (make-final-env (cadr (get-name stmt)) class) class))
      ((eq? (car stmt) 'funcall) (Mstate-fc (M_value (get-name stmt) env class) (cddr stmt) env class))
      ((eq? (car stmt) 'dot) (dot-eval (cadr stmt) (caddr stmt) env class))
      (else (eval-expression stmt env prefix class)))))


(define M_boolean
  (lambda (expression enviornment class)
    (eval-expression-bool expression enviornment prefix class)))


(define M_state_if
  (lambda (statement enviornment return next break continue class)
    (cond
      ((eq? #t (M_boolean (get-cond statement) enviornment class)) (Mstate-stmt (get-body statement) enviornment return break continue next class))
      ((null? (cdddr statement)) (next enviornment))
      (else (Mstate-stmt (cadddr statement) enviornment return break continue next class)))))

;cps
(define Mstate-while
  (lambda (stmt environment ret next class)
	(letrec ((loop (lambda (condition body environment next)
			(if (M_boolean condition environment class)
			(Mstate-stmt body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)) class)
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


(define M_state_cast
  (lambda (stmt env class)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'var) (add (cadr stmt) (M_value (caddr stmt) env class) env))
      ((eq? (car stmt) '=) (myremove (cadr stmt) (M_value (caddr stmt) env class) env (lambda (v) v)));;check here
      (else (error 'assign-error)))))

(define M_state_cast_static
  (lambda (stmt env class)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'static-var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'static-var) (add (cadr stmt) (M_value (caddr stmt) env class) env))
      ((eq? (car stmt) '=) (myremove (cadr stmt) (M_value (caddr stmt) env class) env (lambda (v) v)));;check here
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
      ((or (eq? var #t) (eq? var #f) (number? var)) var)
      ;((null? enviornment) 'undefined)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookup var (cdr enviornment)))
      ((eq? var (caaar enviornment)) (unbox (caadar enviornment)))
      ((null? (caaar enviornment)) (lookup var (cdr enviornment)))
      (else (lookup var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)))))))

(define lookup-super
  (lambda (var enviornment bool)
    (cond
      ((or (eq? var #t) (eq? var #f) (number? var)) var)
      ;((null? enviornment) 'undefined)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookup-super var (cdr enviornment) bool))
      ((and (eq? #t bool) (eq? var (caaar enviornment))) (unbox (caadar enviornment)))
      ((and (eq? #f bool) (eq? var (caaar enviornment))) (lookup-super var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) #t))
      ((null? (caaar enviornment)) (lookup-super var (cdr enviornment)))
      (else (lookup-super var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) bool)))))

(define add
  (lambda (var val enviornment)
    (cons (list (cons var (caar enviornment)) (cons (box val) (cadar enviornment))) (cdr enviornment))))

(define myremove
  (lambda (var val s next)
    (cond
      ((null? s) (error 'undefined-variable))
      ((null? (caar s)) (myremove var val (cdr s) (lambda (v) (next (cons '(()()) v)))))
      ((eq? var (caaar s)) (next (begin (set-box! (caadar s) val) s)))
      (else (myremove var val (cons (list (cdaar s) (cdadar s)) (cdr s)) (lambda (v) (next (cons (list (cons (caaar s) (caar v)) (cons (caadar s) (cadar v))) (cdr v)))))))))


(define eval-expression
  (lambda (expression environment form class)
    ((lambda (operator left-operand right-operand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup expression environment))
         ((eq? '+ (operator expression)) (+ (M_value (left-operand expression) environment class) (M_value (right-operand expression) environment class)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (M_value (cadr expression) environment class)))
         ((eq? '- (operator expression)) (- (M_value (left-operand expression) environment class) (M_value (right-operand expression) environment class)))
         ((eq? '* (operator expression)) (* (M_value (left-operand expression) environment class) (M_value (right-operand expression) environment class)))
         ((eq? '/ (operator expression)) (quotient (M_value (left-operand expression) environment class) (M_value (right-operand expression) environment class)))
         ((eq? '% (operator expression)) (modulo (M_value (left-operand expression) environment class) (M_value (right-operand expression) environment class)))
         
         ((and (not (list? expression)) (number? (lookup expression environment))) (lookup expression environment))
         (else (eval-expression-bool expression environment form class))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))

;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define eval-expression-bool
  (lambda (expression environment form class)
    ((lambda (operator left-operand right-operand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? 'true (car expression)) true)
         ((eq? 'false (car expression)) false)
         ((eq? '&& (operator expression)) (and (M_value  (left-operand expression) environment class) (M_value  (right-operand expression) environment class)))
         ((eq? '! (operator expression))  (not (M_value (cadr expression) environment class)))
         ((eq? '|| (operator expression)) (or (M_value  (left-operand expression) environment class) (M_value  (right-operand expression) environment class)))
         
         ((eq? '== (operator expression)) (equal? (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class)))
         ((eq? '!= (operator expression)) (not (equal? (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class))))
         ((eq? '>= (operator expression)) (>= (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class)))
         ((eq? '> (operator expression)) (> (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class)))
         ((eq? '<= (operator expression)) (<= (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class)))
         ((eq? '< (operator expression)) (< (M_value  (M_value (left-operand expression) environment class) environment class) (M_value  (M_value (right-operand expression) environment class) environment class)))
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


