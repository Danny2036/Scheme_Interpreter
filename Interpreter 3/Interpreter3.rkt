;Interpreter Project Part 3

;Jeff Wagner jpw72

;and Daniel McKinnon dnm30

(load "functionParser.scm")

(define prefix
  (lambda ()
    (list car cadr caddr)))

;Call to parse the file
(define interpret
  (lambda (fileName)
    (wrapper (call/cc
     (lambda (return)
       (mState (mStateList (parser fileName) '((()())) return (lambda (v) v) (lambda (v) v) (lambda (v) v)) return (lambda (v) v) (lambda (v) v) (lambda (v) v)))))))

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
  (lambda (list enviornment return next break continue)
    (cond
      ((null? list) (next enviornment))
      (else (mStateAll (car list) enviornment return break continue (lambda (v) (mStateList (cdr list) v return next break continue)))))))

(define mState
  (lambda (env return break continue next)
    (mStateList (cadr (lookup 'main env)) (newLayerEnv env) return next break continue))) 

;mStateAll feeds to the other mstate functions
(define mStateAll
  (lambda (stmt env return break continue next)
    (cond
      ((or (eq? 'var (car stmt)) (eq? '= (car stmt))) (next (mStateAssign stmt env)))
      ((null? (car stmt)) (mStateAll stmt env env break continue next))
      ((eq? (car stmt) 'return) (return (mValueAll (cadr stmt) env)))
      ((eq? (car stmt) '=) (next (mStateAssign stmt env)))
      ((eq? 'if (car stmt)) (mStateIf stmt env return next break continue))
      ((eq? (car stmt) 'break) (break env))
      ((eq? (car stmt) 'continue) (continue env))
      ((eq? (car stmt) 'while) (mStateWhile stmt env return next))
      ((eq? (car stmt) 'function) (next (mStateFuncDec stmt env return next break continue)))
      ((eq? (car stmt) 'funcall) (begin (mStateFuncCall (getName stmt) (cddr stmt) env) (next env)))
      ((and (null? (cdr stmt)) (eq? (car stmt) 'begin)) (next env))
      ((eq? (car stmt) 'begin) (mStateList (cdr stmt) (newLayerEnv env) return (lambda (v) (next (cdrEnvLayer v))) (lambda (v) (break (cdrEnvLayer v))) (lambda (v) (continue (cdrEnvLayer v)))))
      (else (next env)))))

;mState for executing function bodies
(define mStateFuncBody
  (lambda (actualParam formalParam body env return)
    (cond
      ((not (eq? (length actualParam) (length formalParam))) (error '(Wrong param and arguements)))
      ((null? actualParam) (mStateList body env return (lambda (v) v) (lambda (v) v) (lambda (v) v)))
      (else (mStateFuncBody (cdr actualParam) (cdr formalParam) body (add (car formalParam) (car actualParam) env) return)))))

;mState for function calls
(define mStateFuncCall
  (lambda (name paramlist env)
      (call/cc (lambda (return)
                 (mStateFuncBody (mValueAllParams paramlist env) (car (lookup name env)) (cadr (lookup name env)) ((caddr (lookup name env)) env) return)))))

;mState declaration
(define mStateFuncDec
  (lambda (stmt env ret next break continue)
    (addBinding (getName stmt) (makeClosure (getParamList stmt) (getFuncBody stmt) functionEnv) env)))

;mStateBoolean feeds to the value function
; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
(define mStateBoolean
  (lambda (expression enviornment)
    (mValueBoolean expression enviornment prefix)))

;mStateIf handles if statements
(define mStateIf
  (lambda (statement enviornment return next break continue)
    (cond
      ((eq? #t (mStateBoolean (getCondition statement) enviornment)) (mStateAll (getBody statement) enviornment return break continue next))
      ((null? (cdddr statement)) (next enviornment))
      (else (mStateAll (cadddr statement) enviornment return break continue next)))))

;mStateWhile handles while loops
(define mStateWhile
  (lambda (stmt environment ret next)
	(letrec ((loop (lambda (condition body environment next)
			(if (mStateBoolean condition environment)
			(mStateAll body environment ret (lambda (env) (next env)) (lambda (env) (loop condition body env next)) (lambda (env) (loop condition body env next)))
                        (next environment))))) ;                  break                       continue
	(loop (getCondition stmt) (getBody stmt) environment next))))

;mState for assignment
(define mStateAssign
  (lambda (stmt env)
    (cond
      ((null? stmt) env)
      ((and (eq? (car stmt) 'var) (null? (cddr stmt))) (add (cadr stmt) 'null env))
      ((eq? (car stmt) 'var) (add (cadr stmt) (mValueAll (caddr stmt) env) env))
      ((eq? (car stmt) '=) (myremove (cadr stmt) (mValueAll (caddr stmt) env) env (lambda (v) v)))
      (else (error '(Assignment Error))))))

;///////////////Value Functions//////////////////////////////////////////////
;top level mValue function
(define mValueAllParams
  (lambda (param env)
    (cond
      ((null? param) '())
      (else (cons (mValueAll (car param) env) (mValueAllParams (cdr param) env))))))

;mValue handles boolean values and feeds to function state or feeds other expressions to evaluate
(define mValueAll
  (lambda (stmt env)
    (cond
      ((number? stmt) stmt)
      ((eq? 'true stmt) true)
      ((eq? 'false stmt) false)
      ((not (list? stmt)) (lookup stmt env))
      ((eq? (car stmt) 'funcall) (mStateFuncCall (getName stmt) (cddr stmt) env))
      (else (evaluate stmt env prefix)))))

;gets the values for expressions
(define evaluate
  (lambda (expression environment form)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((null? expression) 0)
         ((number? expression) expression)
         ((not (list? expression)) (lookup expression environment))
         ((eq? '+ (operator expression)) (+ (mValueAll (leftOperand expression) environment) (mValueAll (rightOperand expression) environment)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (mValueAll (cadr expression) environment)))
         ((eq? '- (operator expression)) (- (mValueAll (leftOperand expression) environment) (mValueAll (rightOperand expression) environment)))
         ((eq? '* (operator expression)) (* (mValueAll (leftOperand expression) environment) (mValueAll (rightOperand expression) environment)))
         ((eq? '/ (operator expression)) (quotient (mValueAll (leftOperand expression) environment) (mValueAll (rightOperand expression) environment)))
         ((eq? '% (operator expression)) (modulo (mValueAll (leftOperand expression) environment) (mValueAll (rightOperand expression) environment)))
         ((and (not (list? expression)) (number? (lookup expression environment))) (lookup expression environment))
         (else (mValueBoolean expression environment form))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))

;gets value for boolean expressions
;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define mValueBoolean
  (lambda (expression environment form)
    ((lambda (operator leftOperand rightOperand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? 'true (car expression)) true)
         ((eq? 'false (car expression)) false)
         ((eq? '&& (operator expression)) (and (mValueAll  (leftOperand expression) environment ) (mValueAll  (rightOperand expression) environment )))
         ((eq? '! (operator expression))  (not (mValueAll (cadr expression) environment)))
         ((eq? '|| (operator expression)) (or (mValueAll  (leftOperand expression) environment ) (mValueAll  (rightOperand expression) environment )))
         ((eq? '== (operator expression)) (equal? (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment )))
         ((eq? '!= (operator expression)) (not (equal? (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment ))))
         ((eq? '>= (operator expression)) (>= (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment )))
         ((eq? '> (operator expression)) (> (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment )))
         ((eq? '<= (operator expression)) (<= (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment )))
         ((eq? '< (operator expression)) (< (mValueAll  (mValueAll (leftOperand expression) environment ) environment ) (mValueAll  (mValueAll (rightOperand expression) environment ) environment )))
         (else (error '(Operator is undefined)))))
     (getOperator form) (getLeftOperand form) (getRightOperand form))))

;///////////////////////////Layers, Closures, and enviorments////////////////////////
(define functionEnv
  (lambda (s)
    (cond
      ((null? s) (newLayerEnv s))
      ((null? (cdr s)) (newLayerEnv s))
      (else (functionEnv (cdr s))))))

(define changeClosure
  (lambda (actual closure env)
    (cond
      ((and (null? actual) (null? (closedParam closure))) env)
      ((or (null? actual) (null? (closedParam closure))) (error '(Undefined inside closure)))
      (else (changeClosure (cdr actual)
                           (cons (cdr (closedParam closure)) (cdr closure))
                           (addBinding (car (closedParam closure)) (mValueAll (car actual) env) env))))))
      
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

(define lookup
  (lambda (var enviornment)
    (cond
      ((or (eq? var 'true) (eq? var 'false) (number? var)) var)
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
      ((null? s) (error '(Undefined variable)))
      ((null? (caar s)) (myremove var val (cdr s) (lambda (v) (next (cons '(()()) v)))))
      ((eq? var (caaar s)) (next (begin (set-box! (caadar s) val) s)))
      (else (myremove var val (cons (list (cdaar s) (cdadar s)) (cdr s)) (lambda (v) (next (cons (list (cons (caaar s) (caar v)) (cons (caadar s) (cadar v))) (cdr v)))))))))

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


