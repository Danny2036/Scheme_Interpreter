(load "functionParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

(define interpret
  (lambda (fileName)
    (call/cc
     (lambda (return)
       (Mstate (M_state_list (parser fileName) '((()())) return (lambda (v) v) (lambda (v) v) (lambda (v) v)) return (lambda (v) v) (lambda (v) v) (lambda (v) v))))))

(define M_state_list
  (lambda (list enviornment return next break continue)
    (cond
      ((null? list) (next enviornment))
      (else (Mstate-stmt (car list) enviornment return break continue (lambda (v) (M_state_list (cdr list) v return next break continue)))))))

(define Mstate
  (lambda (env return break continue next)
    (M_state_list (cadr (lookup 'main env)) (new-env-layer env) return next break continue))) 

(define Mstate-stmt
  (lambda (stmt env return break continue next)
    (cond
      ((or (eq? 'var (car stmt)) (eq? '= (car stmt))) (next (M_state_cast stmt env)))
      ((null? (car stmt)) (Mstate-stmt stmt env env break continue next))
      ((eq? (car stmt) 'return) (return (M_value (cadr stmt) env)))
      ((eq? (car stmt) '=) (next (M_state_cast stmt env)))
      ((eq? 'if (car stmt)) (M_state_if stmt env return next break continue))
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (Mstate-while stmt env return next))
      ((eq? (car stmt) 'function) (next (Mstate-fd stmt env return next break continue)))
      ((eq? (car stmt) 'funcall) (begin (Mstate-fc (get-name stmt) (cddr stmt) env) (next env)))
      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (M_state_list (cdr stmt) (new-env-layer env) return (lambda (v) (next (cut-env-layer v))) (lambda (v) (break (cut-env-layer v))) (lambda (v) (continue (cut-env-layer v)))))
      (else (next env)))))


(define f-env
  (lambda (s)
    (cond
      ((null? s) (new-env-layer s))
      ((null? (cdr s)) (new-env-layer s))
      (else (f-env (cdr s))))))


(define Mstate-funcbody
  (lambda (actualparam formalparam body env return)
    (cond
      ((not (eq? (length actualparam) (length formalparam))) (error 'mismatched-param-and-args))
      ((null? actualparam) (M_state_list body env return (lambda (v) v) (lambda (v) v) (lambda (v) v)))
      (else (Mstate-funcbody (cdr actualparam) (cdr formalparam) body (add (car formalparam) (car actualparam) env) return)))))
      
(define mval-paramlist
  (lambda (param env)
    (cond
      ((null? param) '())
      (else (cons (M_value (car param) env) (mval-paramlist (cdr param) env))))))

(define Mstate-fc
  (lambda (name paramlist env)
      (call/cc (lambda (return)
                 (Mstate-funcbody (mval-paramlist paramlist env) (car (lookup name env)) (cadr (lookup name env)) ((caddr (lookup name env)) env) return)))))

(define Mstate-fd
  (lambda (stmt env ret next break continue)
    (addBinding (get-name stmt) (makeClosure (get-param-list stmt) (get-Fbody stmt) f-env) env)))

(define modStmt
  (lambda (stmt)
    (list (car stmt) (cadr stmt))))
    
(define changeClosure
  (lambda (actual closure env)
    (cond
      ((and (null? actual) (null? (closedParam closure))) env)
      ((or (null? actual) (null? (closedParam closure))) (error 'Undefined-in-closure))
      (else (changeClosure (cdr actual)
                           (cons (cdr (closedParam closure)) (cdr closure))
                           (addBinding (car (closedParam closure)) (M_value (car actual) env) env))))))
      
(define closedParam
  (lambda (closure)
    (car closure)))
(define closedBody
  (lambda (closure)
    (cadr closure)))
(define closedF
  (lambda (closure)
    (caddr closure)))


(define addBinding
  (lambda (name val env)
    (add name val env)))
;    (cond 
;      ((null? (cddr env)) (add name val env))
;      (else (myremove name val env (lambda (v) v))))))

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
  (lambda (stmt env)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) true)
      ((eq? 'false stmt) false)
      ((not (list? stmt)) (lookup stmt env))
      ((eq? (car stmt) 'funcall) (Mstate-fc (get-name stmt) (cddr stmt) env))
      (else (eval-expression stmt env prefix)))))

; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
(define M_boolean
  (lambda (expression enviornment)
    (eval-expression-bool expression enviornment prefix)))

(define M_state_if
  (lambda (statement enviornment return next break continue)
    (cond
      ((eq? #t (M_boolean (get-cond statement) enviornment)) (Mstate-stmt (get-body statement) enviornment return break continue next))
      ((null? (cdddr statement)) (next enviornment))
      (else (Mstate-stmt (cadddr statement) enviornment return break continue next)))))

;cps
(define Mstate-while
  (lambda (stmt environment ret next)
	(letrec ((loop (lambda (condition body environment next)
			(if (M_boolean condition environment)
			(Mstate-stmt body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)))
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
  (lambda (stmt env)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'var) (add (cadr stmt) (M_value (caddr stmt) env) env))
      ((eq? (car stmt) '=) (myremove (cadr stmt) (M_value (caddr stmt) env) env (lambda (v) v)));;check here
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
      ((or (eq? var 'true) (eq? var 'false) (number? var)) var)
      ;((null? enviornment) 'undefined)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookup var (cdr enviornment)))
      ((eq? var (caaar enviornment)) (unbox (caadar enviornment)))
      (else (lookup var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)))))))

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



; A MValue function that uses abstraction to allow expressions in prefix,
; postfix, or infix format
; Call as (eval-expression '(3 + 4) '() infix)
(define eval-expression
  (lambda (expression environment form)
    ((lambda (operator left-operand right-operand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup expression environment))
         ((eq? '+ (operator expression)) (+ (M_value (left-operand expression) environment) (M_value (right-operand expression) environment)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (M_value (cadr expression) environment)))
         ((eq? '- (operator expression)) (- (M_value (left-operand expression) environment) (M_value (right-operand expression) environment)))
         ((eq? '* (operator expression)) (* (M_value (left-operand expression) environment) (M_value (right-operand expression) environment)))
         ((eq? '/ (operator expression)) (quotient (M_value (left-operand expression) environment) (M_value (right-operand expression) environment)))
         ((eq? '% (operator expression)) (modulo (M_value (left-operand expression) environment) (M_value (right-operand expression) environment)))
         
         ((and (not (list? expression)) (number? (lookup expression environment))) (lookup expression environment))
         (else (eval-expression-bool expression environment form))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))

;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define eval-expression-bool
  (lambda (expression environment form)
    ((lambda (operator left-operand right-operand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? 'true (car expression)) true)
         ((eq? 'false (car expression)) false)
         ((eq? '&& (operator expression)) (and (M_value  (left-operand expression) environment ) (M_value  (right-operand expression) environment )))
         ((eq? '! (operator expression))  (not (M_value (cadr expression) environment)))
         ((eq? '|| (operator expression)) (or (M_value  (left-operand expression) environment ) (M_value  (right-operand expression) environment )))
         
         ((eq? '== (operator expression)) (equal? (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment )))
         ((eq? '!= (operator expression)) (not (equal? (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment ))))
         ((eq? '>= (operator expression)) (>= (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment )))
         ((eq? '> (operator expression)) (> (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment )))
         ((eq? '<= (operator expression)) (<= (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment )))
         ((eq? '< (operator expression)) (< (M_value  (M_value (left-operand expression) environment ) environment ) (M_value  (M_value (right-operand expression) environment ) environment )))
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


