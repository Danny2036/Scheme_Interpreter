(load "simpleParser.scm")

(define interpret
  (lambda (filename)
    (wrapper (solve (parser filename) intitalState))))

(define intitalState '())

;boolean wrapper

(define wrapper
  (lambda (bool)
    (cond
      ((eq? bool #t) 'true)
      ((eq? bool #f) 'false)
      (else bool))))

;helper functions to make reading code easier
;arithmatatic functions
(define operator car)
(define leftoperand cadr)
(define rightoperand caddr)
      
;assignment functions
(define name cadr)
(define value cadddr)
      
;if function
(define nestedIf caadr)
(define conditional cadr)
(define stmt1 caddr)
(define stmt2 cadddr)

;while functions
(define whileCondition cadr)
(define whileBody caddr)

;solve
(define solve
  (lambda (statement state)
    (cond
      ((not (list? state)) state)
      ((null? statement) state)
      ((null? (cdr statement)) (mStateAll (car statement) state))
      (else (solve (cdr statement) (mStateAll (car statement) state))))))

;mStateAll
(define mStateAll
  (lambda (expression state)
    (cond
      ((null? expression) state)
      ((list? (car expression)) (mStateAll (car expression)  state))
      ((eq? (car expression) 'begin) (mStateBlock (cdr expression) (addLayer state)))
      ((eq? (car expression) 'var) (mStateDeclare expression state))
      ((eq? (operator expression) '=) (mStateAssign expression state))
      ((eq? (car expression) 'return) (mStateReturn expression state))
      ((eq? (car expression) 'if) (mStateIf expression state))
      ((eq? (car expression) 'while) (mStateWhile expression state))
      ((eq? (car expression) 'break) (removeLayer state))
      ((eq? (car expression) 'continue) (mStateWhile expression state))
      (else (mValueAll expression state)))));(error "There is some error in here. Teacher said we don't need to error check, so I left this for you instead")))))

;MStateBlock - assesses a block statement

(define mStateBlock
  (lambda (expression state)
    (cond
      ((null? expression) state)
      (else (mStateAll (cdr expression) (mStateAll (car expression) state))))))


;MStateDeclare: Takes an expression that represents a variable declaration and a state 
;var variable 
;var variable value
(define mStateDeclare
  (lambda (expression state)
    (cond 
      ((null? (cdr (cdr expression))) (add (name expression) '() state))
      ((included (name expression) state) (error "Variable already defined"))
      ((eq? (cadr expression) '=) (add (name expression) (value expression) state))
      (else (add (name expression) (mValueAll (caddr expression) state) state)))))
                                                 ;returns 20 here from mValue
;mStateAssign
;(mStateAssign '(x = 10) '((x 11)))
(define mStateAssign
  (lambda (expression state)
   (cond
     ((not (included (leftoperand expression) state)) (error "Variable not declared"))
     (else(add (leftoperand expression) (mValueAll (rightoperand expression) state) state)))))
       

;mStateReturn
(define mStateReturn
  (lambda (expression state)
    (mValueAll (cdr expression) state)))

;mStateIf (if (conditional) then-statement optional-else-statement)
(define mStateIf
  (lambda (expression state)
    (cond
      ((mValueBoolean (conditional expression) state) (mValueAll (stmt1 expression) state))
      ((null? (cdddr expression)) state)
      ((eq? 'if (car expression)) (mStateIf (cadr expression) state))
      (else (mValueAll (stmt2 expression) state)))))

(define mStateWhile
  (lambda (expression state)
    (if (mValueBoolean (whileCondition expression) state) (mStateWhile expression (mStateAll (whileBody expression) state)) state)))  

;mValueAll
(define mValueAll
  (lambda (expression state)
    (cond     
      ((null? expression) '())
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((included expression state) (lookUpValue expression state))
      ((atom? expression) (lookUpValue expression state))
      ;((list?  (car expression)) (mValueAll (car expression) state))
      ((eq? (car expression) 'return) (mValueAll (cdr expression) state))
      ((eq? (operator expression) '=) (mStateAssign expression state))
      ((isBooleanOperation expression) (mValueBoolean expression state))
      ((isArithmaticOperation expression) (mValueArithmatic expression state))
      (else (mValueAll (car expression) state)))))

;mValueArithmatic: Takes an expression and a state and returns the value of the expression
;(mValueArithmatic '(2 + 2) '((x 2)))
;(mValueArithmatic '(2 + x) '((x 2)))
;(mValueArithmatic 'x '((x true)))
;(mValueArithmatic 'true '((x 2)))
(define mValueArithmatic
  (lambda (expression state)
    (cond
      ((number? expression) expression)
      ((not (list? expression)) (lookUpValue expression state));get variables
      ((and (eq? '- (operator expression)) (null? (cddr expression))) (- 0 (mValueAll (leftoperand expression) state)))
      ((eq? '+ (operator expression)) (+ (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '- (operator expression)) (- (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '* (operator expression)) (* (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '/ (operator expression)) (/ (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '% (operator expression)) (modulo (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((or (eq? 'true expression) (eq? 'true (lookUpValue expression state))) 'true)
      ((or (eq? 'false expression) (eq? 'false (lookUpValue expression state))) 'false)      
      ((number? (car expression)) (car expression))
      (else (error "Unknown operator")))))

;mValueBoolean
;(mValueBoolean '(true && true) '())
;(mValueBoolean '(! true) '())
;(mValueBoolean '(! (! true)) '())
;(mValueBoolean '(true == true) '())
;(mValueBoolean '(false != true) '())
;(mValueBoolean '(4 <= 5) '())
;(mValueBoolean '(4 >= 5) '())
;(mValueBoolean '(4 < 5) '())
;(mValueBoolean '(4 > 5) '())
(define mValueBoolean
  (lambda (expression state)
    (cond
      ((not (list? expression)) (mValueArithmatic expression state))
      ((eq? '&& (operator expression)) (and (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '! (car expression)) (not (mValueAll (cadr expression) state)))
      ((eq? '|| (operator expression)) (or (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '== (operator expression)) (equal? (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '!= (operator expression)) (not (equal? (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state))))
      ((eq? '>= (operator expression)) (>= (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '<= (operator expression)) (<= (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '< (operator expression)) (< (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      ((eq? '> (operator expression)) (> (mValueAll (leftoperand expression) state) (mValueAll (rightoperand expression) state)))
      (else (mValueArithmatic expression state)))))

;isBooleanOperation: returns true if the expression is a boolean operation
(define isBooleanOperation
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((number? (car expression)) #f)
      ((null? (cdr expression)) #f)
      ;((null? (car expression)) #f)
      ((eq? '&& (car expression)) #t)
      ((eq? '! (car expression)) #t) 
      ((eq? '|| (car expression)) #t)
      ((eq? '== (car expression)) #t)
      ((eq? '!= (car expression)) #t)
      ((eq? '>= (car expression)) #t)
      ((eq? '<= (car expression)) #t)
      ((eq? '< (car expression)) #t)
      ((eq? '> (car expression)) #t)
      (else #f))))

;isBooleanOperation: returns true if the expression is a boolean operation
(define isArithmaticOperation
  (lambda (expression)
    (cond
      ((null? expression) #f)
      ((number? (car expression)) #f)
      ((null? (cdr expression)) #f)
      ((eq? '+ (car expression)) #t)
      ((eq? '- (car expression)) #t) 
      ((eq? '/ (car expression)) #t)
      ((eq? '* (car expression)) #t)
      ((eq? '% (car expression)) #t)
      (else #f))))
    
;////////////////////////////// State Related Functions
;lookUpValue: looks up value of variable

(define lookUpValue 
  (lambda (name state)
    (cond
      ((list? name) (lookUpValue (car name) state))
      ((number? name) name) 
      ((null? (nameList state)) '())
      ;((list? (caar state)) (cons (lookUpValue name (car state)) (lookUpValue name (cdr state))))
      ((and (list? (caar state)) (included name (caar state))) (lookUpValue name (car state)))
      ((and (list? (caar state)) (not (included name (caar state)))) (lookUpValue name (cdr state)))
      ((equals* name (firstName state))  (firstValue state)) 
      (else (lookUpValue name (incState state))))))

;removeState: removes binding in a state
(define removeState
  (lambda (name state)
    (cond
      ((null? (nameList state)) state)
      ((and (atom? (firstName state)) (eq? (firstName state) name)) (incState state))
      ((and (atom? (firstName state)) (not (eq? (firstName state) name))) (joinStates (obama state) (removeState name (incState state))))
      ;((eq? name (firstName state)) (incState state))
      ((list? (nameList state)) (cons (removeState name (nameList state)) (removeState name (cdr state))))
      ((not (included name state)) state)
      (else (joinStates (obama state) (removeState name (incState state)))))))

;add: adds value into the state
;add now automatically adds to the top layer
(define add
  (lambda (name value state)
    (cond
      ((null? (nameList state)) (cons (cons name (nameList (initState))) (cons (cons value (valueList (initState))) '())))
      ((list? (caar state)) (cons (add name value (nameList state)) (cdr state)))
      ((included name state) (cons (cons name (nameList (removeState name state))) (cons (cons value (valueList (removeState name state))) '())))
      (else (cons (cons name (nameList state)) (cons (cons value (valueList state)) '()))))))

;initial state
(define initState
  (lambda()
    '(() ())))

;name list of state
(define nameList
  (lambda (state)
    (cond
      ((null? state) '())
      (else (car state)))))

;first name in the state
(define firstName
  (lambda (state)
    (if (null? (nameList state))
        '()
        (car (nameList state)))))

;value list of state
(define valueList
  (lambda (state)
    (cadr state)))

;first value in the state
(define firstValue
  (lambda (state)
    (if (null? (valueList state))
        '()
        (car (valueList state)))))

;head of state
(define obama
  (lambda (state)
    (cons (cons (firstName state) '()) (cons (cons (firstValue state)'()) '()))))

;move down the list in state
(define incState
  (lambda (state)
    (cond
      ((null? (nameList state)) (initState))
      ;((null? (cdr (nameList state))) (initState))
      (else (cons (cdr (nameList state)) (cons (cdr (valueList state)) '())))))) 

;combine two states together
(define joinStates
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((null? s2) s1)
      (else (cons (append (nameList s1) (nameList s2)) (cons (append (valueList s1) (valueList s2)) '()))))))


;simple check to see if atom is in name list

(define included
  (lambda (atom state)
    (cond
      ((null? (nameList state)) #f)      
      ((pair? atom) #f)
      ((and (atom? (nameList state)) (equals* (nameList state) atom)) #t)
      ((and (atom? (nameList state)) (not (equals* (nameList state) atom))) (included atom (cdr state)))                    
      ((equals* (car (nameList state)) atom) #t)
      ((list? (nameList state)) (included atom (nameList state)))
      (else (included atom (incState state))))))

;//////////////layer functions////////////

;add new layer to the state

(define addLayer
  (lambda (state)
    (cons (initState) state)))

;remove top layer from the state
;safeguarded so won't pop off lowest level

(define removeLayer
  (lambda (state)
    (cond
      ((null? (car state)) state)
      ((and (list? (car state)) (atom? (caar state))) state)
      ((list? (caar state)) (cdr state))
      (else state))))

;get the top layer

(define topLayer
  (lambda (state)
    (if (list? (caar state))  ;another safeguard
        (car state)
        (topLayer state))))
    
;equalization helpers
;assuming nulls are equal for simplicity

(define equals*
  (lambda (term1 term2)
    (cond
      ((and (null? term1) (null? term2)) #t)
      ((and (null? term1) (not (null? term2))) #f)
      ((and (null? term2) (not (null? term1)) #f))
      ((and (atom? term1) (list? term1)) #f)
      ((and (atom? term2) (list? term2)) #f)
      ((and (atom? term1) (atom? term2)) (eq? term1 term2))
      ((and (list? term1) (list? term2)) (eq-list? term1 term2))
      (else #f))))
                                         
(define eq-list?
  (lambda (l1 l2)
    (cond
      ((or (null? l1) (null? l2)) (and (null? l1) (null? l2)))
      ((and (pair? (car l1)) (pair? (car l2))) (and (eq-list? (car l1) (car l2))
                                                    (eq-list? (cdr l1) (cdr l2))))
      ((or (pair? (car l1)) (pair? (car l2))) #f)
      ((eq? (car l1) (car l2)) (eq-list? (cdr l1) (cdr l2)))
      (else #f))))

;//////////////////////////////

;Used to test for if something is an atom
(define (atom? x) (not (or (pair? x) (null? x))))



;////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
;Attempt at making mState functions cps. Does not work
;
;;mStateAll
;(define mStateAll
;  (lambda (expression state return)
;    (cond
;      ((null? expression) (return state))
;      ((list? (car expression)) (mStateAll (car expression) (lambda (v) (return v))))
;      ((eq? (car expression) 'begin) (mStateBlock (cdr expression) (lambda (v) (return (addLayer v))))
;      ((eq? (car expression) 'var) (mStateDeclare expression state) (lambda (v) (return v)))
;      ((eq? (operator expression) '=) (mStateAssign expression state) (lambda (v) (return v)))
;      ((eq? (car expression) 'return) (mStateReturn expression state) (lambda (v) (return v)))
;      ((eq? (car expression) 'if) (mStateIf expression state) (lambda (v) (return v)))
;      ((eq? (car expression) 'while) (mStateWhile expression state) (lambda (v) (return v)))
;      ((eq? (car expression) 'break) (removeLayer state) (lambda (v) (return v)))
;      ((eq? (car expression) 'continue) (mStateWhile expression state) (lambda (v) (return v)))
;      (else (mValueAll expression state))))));(error "There is some error in here. Teacher said we don't need to error check, so I left this for you instead")))))
;
;;MStateBlock - assesses a block statement
;
;(define mStateBlock
;  (lambda (expression state return)
;    (cond
;      ((null? expression) (return state))
;      (else (mStateAll (cdr expression) (mStateAll (car expression) state (lambda (v) (return v))) (lambda (v) (return v)))))))
;
;
;;MStateDeclare: Takes an expression that represents a variable declaration and a state 
;;var variable 
;;var variable value
;(define mStateDeclare
;  (lambda (expression state return)
;    (cond 
;      ((null? (cdr (cdr expression))) (return (add (name expression) '() state)))
;      ((included (name expression) state) (error "Variable already defined"))
;      ((eq? (cadr expression) '=) (return (add (name expression) (value expression) state)))
;      (else (add (name expression) (mValueAll (caddr expression) state (lambda (v) (return v) state)))))))
;                                                 ;returns 20 here from mValue
;;mStateAssign
;;(mStateAssign '(x = 10) '((x 11)))
;(define mStateAssign
;  (lambda (expression state return)
;   (cond
;     ((not (included (leftoperand expression) state)) (error "Variable not declared"))
;     (else (add (leftoperand expression) (mValueAll (rightoperand expression) state (lambda (v) (return v)) state))))))
;       
;
;;mStateReturn
;(define mStateReturn
;  (lambda (expression state return)
;    (mValueAll (cdr expression) state (lambda (v) (return v)))))
;
;;mStateIf (if (conditional) then-statement optional-else-statement)
;(define mStateIf
;  (lambda (expression state return)
;    (cond
;      ((mValueBoolean (conditional expression) state) (return (mValueAll (stmt1 expression) state)))
;      ((null? (cdddr expression)) state)
;      ((eq? 'if (car expression)) (mStateIf (cadr expression) state (lambda (v) (return v))))
;      (else (return (mValueAll (stmt2 expression) state))))))
;
;(define mStateWhile
;  (lambda (expression state return)
;    (if (mValueBoolean (whileCondition expression) state) (mStateWhile expression (mStateAll (whileBody expression) state (lambda (v2) (return v2)) state (lambda (v) (return v)))))))  
