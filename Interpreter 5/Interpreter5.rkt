;Interpreter Project Part 5

;Jeff Wagner jpw72

;and Daniel McKinnon dnm30

;note: unfortunately we are getting an error in our getFieldEnv function where our environment is losing proper form 
;that we are unable to figure out, but we wanted to turn in what we have for this part

(load "classParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

;Call to parse the file
(define interpret
  (lambda (fileName class)
    (wrapper (call/cc
     (lambda (return)
       (mStateAll (mStateClassList (parser fileName) (newEnv) return (lambda (v) v) (lambda (v) (mStateList (caddr catch) (add (cadr catch) v (newLayerEnv env)) return (lambda (v) (next (cdrEnvLayer v))) (lambda (v) (break (cdrEnvLayer v))) (continue (cdrEnvLayer v)) v))) return (lambda (v) v) (lambda (v) v) (lambda (v) v) (string->symbol class) (lambda (v) (mStateList (caddr catch) (add (cadr catch) v (newLayerEnv env)) return (lambda (v) (next (cdrEnvLayer v))) (lambda (v) (break (cdrEnvLayer v))) (continue (cdrEnvLayer v))) v)))))))



;boolean wrapper, changes #t to true, #f to false, or does nothing
(define wrapper
  (lambda (bool)
    (cond
      ((eq? bool #t) 'true)
      ((eq? bool #f) 'false)
      (else bool))))

;////////////////////State Functions/////////////////////////////
;mStateList top level mState function
(define mStateList
  (lambda (list enviornment return next break continue class try)
    (cond
      ((null? list) (next enviornment))
      (else (mStateAll (car list) enviornment return break continue (lambda (v) (mStateList (cdr list) v return next break continue class try)) class try)))))

(define mState
  (lambda (name env return break continue next class try)
    (mStateList (cadr (lookupFunction name 'main env)) (finalEnvironment name env) return next break continue class try)))

;mStateAll feeds to the other mstate functions
(define mStateAll
  (lambda (stmt env return break continue next class try)
    (cond
      ((or (eq? 'var (car stmt)) (eq? '= (car stmt))) (next (mStateAssign stmt env class try)))
      ((null? (car stmt)) (mStateAll stmt env env break continue next class try))
      ((eq? (car stmt) 'return) (return (mValueAll (cadr stmt) env class try)))
      ((eq? (car stmt) '=) (next (mStateAssign stmt env class try)))
      ((eq? 'if (car stmt)) (mStateIf stmt env return next break continue class try))
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (mStateWhile stmt env return next class try))
      ((eq? (car stmt) 'function) (next (mStateFuncDec stmt env return next break continue try)))
      ((eq? (car stmt) 'funcall) (begin (mStateFuncCall (getName stmt) (cddr stmt) env class try) (next env)))      
      ((eq? (car stmt) 'class) (begin (set! env (add (cadr stmt) (newClass stmt env) env)) (mStateList (cadddr stmt) env return next break continue (cadr stmt) try)))
      ((eq? (car stmt) 'static-function) (setFunctionEnv class (mStateFuncDec stmt (getFunctionEnv class env) return next break continue) env next))
      ((eq? (car stmt) 'static-var) (setFieldEnv class (mStateAssign stmt (getFieldEnv class env) class try) env next))
      ((eq? (car stmt) 'dot) (dot-eval (cadr stmt) (caddr stmt) env class))
      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (mStateList (cdr stmt) (newLayerEnv env) return (lambda (v) (next (cdrEnvLayer v))) (lambda (v) (break (cdrEnvLayer v))) (lambda (v) (continue (cdrEnvLayer v))) class try))
      ((eq? (car stmt) 'try) (next (mStateTry (cadr stmt) (caddr stmt) (cadr (cadddr stmt)) return break continue next class try)))
      (else (next env)))))

;mState for executing function bodies
(define mStateFuncBody
  (lambda (actualParam formalParam body env return class try)
    (cond
      ((not (eq? (length actualParam) (length formalParam))) (error '(Wrong param and arguements)))
      ((null? actualParam) (mStateList body env return (lambda (v) v) (lambda (v) v) (lambda (v) v) class try))
      (else (mStateFuncBody (cdr actualParam) (cdr formalParam) body (add (car formalParam) (car actualParam) env) return class try)))))

;mState for function calls
(define mStateFuncCall
  (lambda (name paramlist env class try)
      (call/cc (lambda (return)
                 (mStateFuncBody (mValueAllParams paramlist env class try) (car (lookup name env)) (cadr (lookup name env)) ((caddr (lookup name env)) env) return class try)))))

;mState declaration
(define mStateFuncDec
  (lambda (stmt env ret next break continue)
    (addBinding (getName stmt) (makeClosure (getParamList stmt) (getFuncBody stmt) functionEnv) env)))

;mStateBoolean feeds to the value function
; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
(define mStateBoolean
  (lambda (expression enviornment class try)
    (mValueBoolean expression enviornment prefix class try)))

;mStateIf handles if statements
(define mStateIf
  (lambda (statement enviornment return next break continue class try)
    (cond
      ((eq? #t (mStateBoolean (getCondition statement) enviornment class try)) (mStateAll (getBody statement) enviornment return break continue next class try))
      ((null? (cdddr statement)) (next enviornment))
      (else (mStateAll (cadddr statement) enviornment return break continue next class try)))))

;mStateWhile handles while loops
(define mStateWhile
  (lambda (stmt environment ret next try)
	(letrec ((loop (lambda (condition body environment next)
			(if (mStateBoolean condition environment try)
			(mStateAll body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)) try)
                        (next environment))))) 
	(loop (getCondition stmt) (getBody stmt) environment next))))

;mState for assignment
(define mStateAssign
  (lambda (stmt env class try)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (modInstance (cadr stmt) 'null env class try))
      ((eq? (car stmt) 'var) (modInstance (cadr stmt) (mValueAll (caddr stmt) env class try) env class try))
      ((eq? (car stmt) '=) (begin (myRemoveIn (cadr stmt) (mValueAll (caddr stmt) env class try) env class try) env))
      ((and (eq? (car stmt) 'static-var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'static-var) (add (cadr stmt) (mValueAll (caddr stmt) env class try) env))
      (else (error '(Assignment Error))))))


;mState functions for try stuff
(define mStateTry
  (lambda (tryBody catch finallyBody return break continue next class environment)
    (cond
      ((null? finallyBody) (mStateDoTry tryBody catch return break continue next class environment))
      (else (begin (mStateDoTry tryBody catch return break continue next class environment) (mStateList finallyBody (newLayerEnv environment) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (lambda (v) (continue (cdrEnvlayer v))) class class environment))))))

(define mStateDoTry
  (lambda (tryBody catch return break continue next class environment)
    (mStateList tryBody (newLayerEnv environment) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (lambda (v) (continue (cdrEnvlayer environment))) (lambda (v) (mStateList (caddr catch) (add (cadr catch) v (newLayerEnv env)) return (lambda (v) (next (cdrEnvlayer v))) (lambda (v) (break (cdrEnvlayer v))) (continue (cdrEnvlayer v))) class class environment) class class environment)))


;instance funcdef
(define instanceFuncDec
  (lambda (stmt env class try)
    (myRemoveIn 'instance (mStateFuncDef stmt (lookupIn 'instance class env)) env class try)))

 
(define mStateClassList
  (lambda (classes env return next try)
    (cond
      ((null? classes) (next env))
      ((not (eq? (caar classes) 'class)) (error "Invalid declaration"))
      ((null? (caddar classes)) (next (mStateClassList (cdr classes) (add (cadar classes) (mStateList (car (cdddar classes)) (add (cadar classes) (newInstanceEnv (cadar classes)) env) return next (lambda (v) v) (lambda (v) v) (cadar classes) try) try) env) return next))
      (else (mStateClassList (cdr classes) (add (cadar classes) (list (car (mStateList (car (cdddar classes)) (addSuper (lookup (cadr (caddar classes)) env) (add 'super (cadr (caddar classes)) (add 'this (cadar classes) (add 'instance (lookupIn 'instance (cadr (caddar classes)) env) (newLayerEnv env))))(cadar classes)) return next (lambda (v) v) (lambda (v) v) 'null try))) env) return next)))))



;///////////////////////Value Functions/////////////////////////////
;top level mValue function
(define mValueAllParams
  (lambda (param env class)
    (cond
      ((null? param) '())
      (else (cons (mValueAll (car param) env class try) (mValueAllParams (cdr param) env class try))))))

;mValue handles boolean values and feeds to function state or feeds other expressions to evaluate
(define mValueAll
  (lambda (stmt env class try)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) true)
      ((eq? 'false stmt) false)
      ((not (list? stmt)) (lookup stmt env))
      ((and (eq? (car stmt) 'funcall) (list? (cadr stmt))) (mStateFuncCall (caddr (getName stmt)) (cddr stmt) (finalEnvironment (cadr (getName stmt)) class) class try))
      ((eq? (car stmt) 'funcall) (mStateFuncCall (mValueAll (getName stmt) env class) (cddr stmt) env class try))
      ((eq? (car stmt) 'dot) (dotOp (cadr stmt) (caddr stmt) env class))
      (else (evaluate stmt env prefix class try)))))



;gets the values for expressions
(define evaluate
  (lambda (expression environment form class try)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup expression environment))
         ((eq? '+ (operator expression)) (+ (mValueAll (leftOperand expression) environment class try) (mValueAll (rightOperand expression) environment class try)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (mValueAll (cadr expression) environment class try)))
         ((eq? '- (operator expression)) (- (mValueAll (leftOperand expression) environment class try) (mValueAll (rightOperand expression) environment class try)))
         ((eq? '* (operator expression)) (* (mValueAll (leftOperand expression) environment class try) (mValueAll (rightOperand expression) environment class try)))
         ((eq? '/ (operator expression)) (quotient (mValueAll (leftOperand expression) environment class try) (mValueAll (rightOperand expression) environment class try)))
         ((eq? '% (operator expression)) (modulo (mValueAll (leftOperand expression class try) environment) (mValueAll (rightOperand expression) environment class try)))
         ((and (not (list? expression)) (number? (lookup expression environment))) (lookup expression environment))
         ((and (eq? 'funcall (operator expression)) (list? (leftOperand expression))) (mstateFuncCall (car (cddadr expression)) (cddr expression) environment class (convertClass (cadadr expression) class environment) try))
         ((eq? 'funcall (operator expression)) (mStateFuncCall (getName expression) (cddr expression) environment class class try))         
         ((eq? 'new (operator expression)) (lookup (leftOperand expression) environment))        
         ((and (eq? 'dot (operator expression)) (eq? 'this (leftOperand expression))) (evaluate (lookupIn (rightOperand expression) (lookupIn 'this class environment) environment) environment form (lookupIn 'this class environment)))
         ((and (eq? 'dot (operator expression)) (eq? 'super (left-operand expression))) (evaluate (lookupIn (rightOperand expression) (lookupIn 'super class environment) environment) environment form (lookupIn 'super class environment)))
         ((eq? 'dot (operator expression)) (evaluate (lookupIn (rightOperand expression) (leftOperand expression) environment) environment form (leftOperand expression)))
         (else (mValueBoolean expression environment form class try))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))


;gets value for boolean expressions
;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define mValueBoolean
  (lambda (expression environment form class try)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? #t (car expression)) true)
         ((eq? #f (car expression)) false)
         ((eq? 'true (car expression)) true)
         ((eq? 'false (car expression)) false)
         ((eq? '&& (operator expression)) (and (mValueAll  (leftOperand expression) environment class try) (mValueAll  (rightOperand expression) environment class try)))
         ((eq? '! (operator expression))  (not (mValueAll (cadr expression) environment class try)))
         ((eq? '|| (operator expression)) (or (mValueAll  (leftOperand expression) environment class try) (mValueAll  (rightOperand expression) environment class try)))
         ((eq? '== (operator expression)) (equal? (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try)))
         ((eq? '!= (operator expression)) (not (equal? (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try))))
         ((eq? '>= (operator expression)) (>= (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try)))
         ((eq? '> (operator expression)) (> (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try)))
         ((eq? '<= (operator expression)) (<= (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try)))
         ((eq? '< (operator expression)) (< (mValueAll  (mValueAll (leftOperand expression) environment class try) environment class try) (mValueAll  (mValueAll (rightOperand expression) environment class try) environment class try)))
         (else (error '(Operator is undefined)))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))




;///////////////////////////Class stuff /////////////////////////

(define newClass
  (lambda (stmt env)                 
    (cond                
      ((null? (caddr stmt)) (list '((()())) '((()()))  '((()())) '(()()))) ;basic format
      (else (list (getFunctionEnv (getExtends stmt) env)
                  (getFieldEnv (getExtends stmt) env)
                  (getInstanceEnv (getExtends stmt) env)
                  (getParent (getExtends stmt) env))))))

;handles extending
(define getExtends
  (lambda (stmt)
    (car (cdaddr stmt))))

;parent
(define getParent
  (lambda (class env)
    (cadddr (lookup class env))))

;getters and setters for the parts of the environment containing the different parts of class
(define getFunctionEnv
  (lambda (class env)
    (car (lookup class env))))

(define setFunctionEnv
  (lambda (class new env next)
    (myremove class (list new (getFieldEnv class env) (getInstanceEnv class env) (getParent class env)) env next)))
    
(define getFieldEnv
  (lambda (class env)
    (car (cdr (lookup class env)))))

(define setFieldEnv
  (lambda (class new env next )
    (myremove class (list (getFunctionEnv class env) new (getInstanceEnv class env) (getParent class env)) env next)))

(define getInstanceEnv
  (lambda (class env)
    (caddr (lookup class env))))

(define setInstanceEnv
  (lambda (class new env next)
    (myremove class (list (getFunctionEnv class env) (getFieldEnv class env) new (getParent class env)) env next)))
 
(define convertclass
  (lambda (newclass oldclass env)
    (cond
      ((eq? newclass 'this) (lookupIn newclass oldclass env))
      ((eq? newclass 'super)(lookupIn newclass oldclass env))
      (else newclass try))))

(define dotOp
  (lambda (name called env class)
    (cond
      ((eq? 'super name) (lookupSuper called env #f))
      (else (lookup called (finalEnvironment name env))))))




;///////////////////////////Layers Closures Enviorments Instances///////////////////////////
(define newEnv
  (lambda ()
    '((()()))))

(define finalEnvironment
  (lambda (class env)
    (append
     (list (car (getFunctionEnv class env))
          (car (getFieldEnv class env))
          (car (getInstanceEnv class env))
          (getParent class env))
     env)))

(define mainEnv
  (lambda (class env)
    (lookup class env)))

(define functionEnv
  (lambda (s)
    (cond
      ((null? s) (newLayerEnv s))
      ((null? (cdr s)) (newLayerEnv s))
      (else (functionEnv (cdr s))))))

(define changeClosure
  (lambda (actual closure env class)
    (cond
      ((and (null? actual) (null? (closedParam closure))) env)
      ((or (null? actual) (null? (closedParam closure))) (error '(Undefined inside closure)))
      (else (changeClosure (cdr actual)
                           (cons (cdr (closedParam closure)) (cdr closure))
                           (addBinding (car (closedParam closure)) (mValueAll (car actual) env class try) env))))))
      
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

(define makeClosure
  (lambda (args body Fstate)
    (list args body Fstate)))

(define newLayerEnv
  (lambda (env)
    (cons '(()()) env)))

(define cdrEnvLayer
  (lambda (env)
    (cdr env)))    

;lookup variable
(define lookup
  (lambda (var enviornment)
    (cond
      ((or (eq? var 'true) (eq? var 'false) (number? var)) var)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookup var (cdr enviornment)))
      ((eq? var (caaar enviornment)) (unbox (caadar enviornment)))
      ((null? (caaar enviornment)) (lookup var (cdr enviornment)))
      ((eq? 'instance (caaar enviornment)) (lookupInInstance var enviornment))
      (else (lookup var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)))))))

(define lookupFunction
  (lambda (class var env)
    (lookup var (getFunctionEnv class env))))

;super functionality
(define addSuper
  (lambda (vars env class try)
    (cond
      ((null? (caar vars)) env)
      ((eq? (caaar vars) 'this) env)
      ((eq? (caaar vars) 'instance) env)
      (else (addSuper (list (list (cdaar vars) (cdadar vars))) (list (list (cons (caaar vars) (caar env)) (cons (caadar vars) (cadar env)))) class try)))))

(define lookupSuper
  (lambda (var enviornment bool)
    (cond
      ((or (eq? var #t) (eq? var #f) (number? var)) var)
      ((null? enviornment) (error 'undefined))
      ((null? (caar enviornment)) (lookupSuper var (cdr enviornment) bool))
      ((and (eq? #t bool) (eq? var (caaar enviornment))) (unbox (caadar enviornment)))
      ((and (eq? #f bool) (eq? var (caaar enviornment))) (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) #t))
      ((and (eq? 'true bool) (eq? var (caaar enviornment))) (unbox (caadar enviornment)))
      ((and (eq? 'false bool) (eq? var (caaar enviornment))) (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) #t))
      ((null? (caaar enviornment)) (lookupSuper var (cdr enviornment)))
      (else (lookupSuper var (cons (list (cdaar enviornment) (cdadar enviornment)) (cdr enviornment)) bool)))))



(define add
  (lambda (var val enviornment)
    (cons (list (cons var (caar enviornment)) (cons (box val) (cadar enviornment))) (cdr enviornment))))

;remove
(define myremove
  (lambda (var val env next)
    (cond
      ((null? env) (error '(Undefined variable)))
      ((null? (caar env)) (myremove var val (cdr env) (lambda (v) (next (cons '(()()) v)))))
      ((eq? var (caaar env)) (next (begin (set-box! (caadar env) val) env)))
      (else (myremove var val (cons (list (cdaar env) (cdadar env)) (cdr env)) (lambda (v) (next (cons (list (cons (caaar env) (caar v)) (cons (caadar env) (cadar v))) (cdr v)))))))))

;remove inside class, makes sure variable isnt missed
(define myRemoveIn
  (lambda (var val env class try)
    (cond
      ((null? (myremove var val env (lambda (v) v))) (myremove var val (lookup class env) (lambda (v) v)))
      (else (myremove var val env (lambda (v) v))))))


;formats instance
(define newInstanceEnv
  (lambda (class)
    (list (list (list 'this 'instance) (list (box class) (box (newEnv)))))))


;instance in environment
(define lookupInstance
  (lambda (env)
    (lookup 'instance env)))

(define lookupInInstance
  (lambda (var env)
    (lookup var (lookupInstance env))))


(define modInstance
  (lambda (var val env class try)
    (myRemoveIn 'instance (add var val (lookupIn 'instance class env)) env class try)))



;/////////////////////General Helpers//////////////////////////////
(define getName
  (lambda (stmt)
    (cadr stmt)))
(define getParamList
  (lambda (stmt)
    (caddr stmt)))
(define getFuncBody
  (lambda (stmt)
    (cadddr stmt)))

(define getCondition
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (cadr stmt))
      ((eq? 'if (car stmt)) (cadr stmt)))))

(define getBody
  (lambda (stmt)
    (cond
      ((eq? 'while (car stmt)) (caddr stmt))
      ((eq? 'if (car stmt)) (caddr stmt)))))

; retrieves the operator function from the format
(define getOperator
  (lambda (form)
    (car (form))))

; retrieves the leftOperand function from the format
(define getLeftOperand
  (lambda (form)
    (cadr (form))))

; retrieves the rightOperand function from the format
(define getRightOperand
  (lambda (form)
    (caddr (form))))




