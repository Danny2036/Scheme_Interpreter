(load "simpleParser.scm")

;This is just me defining the null state. This is more for making testing easy than anything
(define nullState '(()()))

;Another test method to make sure the parser is working in the manner expected. 
(define returnParse
  (lambda (file)
    (parser file)))

;Checks if a binding already exists in the state, if it does not exist it will add the binding to the state. If the binding already exists, the state given is returned. 
(define Add
  (lambda (state variable value)
    (cond
      ((null? (car state)) (list (list variable) (list value)))
      ((eq? (caar state) variable) state)
      (else ((lambda (resultstate) 
               (list (cons (caar state) (car resultstate)) (cons (caadr state) (cadr resultstate)))) 
               (Add (list (cdar state) (cdadr state)) variable value))))))

;Will remove the binding from the state if it exists. If there is no binding it will return the given state. 
(define Remove
  (lambda (state variable)
    (cond
      ((null? (car state)) state)
      ((eq? (caar state) variable) (Remove (list (cdar state) (cdadr state)) variable))
      (else ((lambda (resultstate)
              (list (cons (caar state) (car resultstate)) (cons (caadr state) (cadr resultstate)))) 
              (Remove (list (cdar state) (cdadr state)) variable))))))

;Write lookup. Follows similar pattern to add/remove. If variable exists, return the value, if it does not exist, throw an error. 
(define lookup
  (lambda (state variable)
    (cond
      ((null? (car state)) (error "Variable" variable "was not in the given state."))
      ((eq? (caar state) variable) (caadr state))
      (else (lookup (list (cdar state) (cdadr state)) variable)))))

;///////////////////////////////////////////DECLARE SECTION/////////////////////////////////////////////////////////////

;MStateDeclare will check if a variable was already declared. If it was, it will error, if not it will add it to the state table.
(define MStateDeclare
 (lambda (statement state)
   (cond
     ((hasBeenDeclared (declareGetVariable statement) state) (error (declareGetVariable statement) "has already been declared"))
     (else (Add state (declareGetVariable statement) (MValue (declareGetValue statement) state))))))
   
;Helper for declare to check if a variable name has already been declared                                                   
(define hasBeenDeclared 
  (lambda (varname state)
      (if (not (member varname (car state))) #f #t))) 
      
;Helper for declare to return the variable name in a declare statement
(define declareGetVariable cadr)

;Helper for declare to return the value of the variable declared. If there was no assignment, it returns the null list
(define declareGetValue
  (lambda (l)
    (if (null? (cddr l))
        '()
        (caddr l))))

;/////////////////////////////////////////ASSIGN SECTION/////////////////////////////////////////////////////////////

;MState assign will add the new value to a variable if it has already been assigned. If it is not found in the state, remove will throw an error
(define MStateAssign
  (lambda (statement state)
    (if (not (hasBeenDeclared (declareGetVariable statement) state)) (error (declareGetVariable statement) "has not yet been declared"))
    (Add (Remove state (declareGetVariable statement)) (declareGetVariable statement) (MValue (declareGetValue statement) state))))

; MValueArith evaluates arithmetic expressions
(define MValueArith
  (lambda (expression state)
      (cond
        ((eq? '+ (operator expression)) (+ (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '- (operator expression)) ((lambda (leftval rightval)
                                           (if (null? rightval) (- 0 leftval)
                                               (- leftval rightval)))(MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '/ (operator expression)) (quotient (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '* (operator expression)) (* (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '% (operator expression)) (remainder (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        (else (error 'bad-operator)))))

;MStateBoolean will evaluate a boolean statement and return true or false
(define MValueBoolean
  (lambda (expression state)
      (cond
        ((eq? '== (operator expression)) (eq? (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '!= (operator expression)) (not (eq? (MValue (leftoperand expression) state) (MValue (rightoperand expression) state))))
        ((eq? '< (operator expression)) (< (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '> (operator expression)) (> (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '<= (operator expression)) (<= (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '>= (operator expression)) (>= (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '&& (operator expression)) (and (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '|| (operator expression)) (or (MValue (leftoperand expression) state) (MValue (rightoperand expression) state)))
        ((eq? '! (operator expression)) (not (MValue (leftoperand expression) state))) 
        (else (error 'bad-operator)))))

;The generic MValue method which will call MValueArith if it is an arithmetic expression or MValueBoolean if it is a boolean expression.
(define MValue
  (lambda (expression state)
    (cond
      ((null? expression) '())
      ((number? expression) expression)
      ((eq? 'true expression) #t)
      ((eq? 'false expression) #f)
      ((atom? expression) (lookup state expression)) 
      ((member (operator expression) '(+ - / * %)) (MValueArith expression state))
      ((member (operator expression) '(== != < > <= >= && || !)) (MValueBoolean expression state)))))

;MStateReturn will take a return statement and return the statement right of "return"
(define MValueReturn
  (lambda (expression state)
    (MValue (cadr expression) state)))

;Helper to get the operator from an expression in prefix notation
(define operator car)
   
;Helper to get the left operand from an expression in prefix notation
(define leftoperand cadr) 

;Helper to get the right operand from an expression in prefix notation
(define rightoperand 
  (lambda (expression)
    (if (null? (cddr expression))
        '()
        (caddr expression))))

;Used to test for if something is an atom
(define (atom? x) (not (or (pair? x) (null? x))))

;//////////////////////////////////////IF STATEMENT SECTION///////////////////////////////////////

;MValueIf will check if the if condition is true, if so it'll evaluate the statement in the if, otherwise it'll evalueate the statement in the else
(define MValueIf
  (lambda (statement state)
    (if (MValue (ifStmtGetCond statement) state)
        (valueStatement (ifStmtGetThenStmt statement) state)
        (valueStatement (ifStmtGetOptionalElse statement) state))))

;Computes the state change that happens by evaluating an if statement and any substatements
(define MStateIf
  (lambda (statement state)
    (if (MValue (ifStmtGetCond statement) state)
        (stateStatement (ifStmtGetThenStmt statement) state)
        (stateStatement (ifStmtGetOptionalElse statement) state))))

;Helper for return to get the expression returned 
(define returnGetExpression cadr)

;Helper for if statement to get the first condition
(define ifStmtGetCond cadr)

;Helper for if statment to get the "then" statement
(define ifStmtGetThenStmt caddr)

;Helper for if statement to get the optional else statement
(define ifStmtGetOptionalElse
  (lambda (l)
    (if (null? (cdddr l))
        '()
        (cadddr l))))

;Value will loop through the parsed code and send valueStatement each statement one by one. This is basically looping through the parsing tree and evaluating each statement along the way
(define value
  (lambda (parsetree state)
    (cond
       ((null? parsetree) state)
    (else ((lambda (result newState)
    (if (null? result) (value (cdr parsetree) newState)
        (cond
          ((eq? result #t) 'true)
          ((eq? result #f) 'false)
          (else result))))
        (valueStatement (car parsetree) state)
        (stateStatement (car parsetree) state))))))

;valueStatement will take in a statement, determine what kind of statement it is, then run the Mvalue of the statement using the Mstate of the statement
(define valueStatement
  (lambda (statement state)
    (cond
      ((null? statement) '())
      ((eq? (car statement) 'if) (MValueIf statement state))
      ((eq? (car statement) 'return) (MValueReturn statement state))
      (else '()))))

;valueState will check a statement to see which kind of statement it is and change the state accordingly
(define stateStatement
  (lambda (statement state)
    (cond
      ((null? statement) state)
      ((eq? (car statement) 'if) (MStateIf statement state))
      ((eq? (car statement) 'var) (MStateDeclare statement state))
      ((eq? (car statement) 'return) state)
      ((eq? (car statement) '=) (MStateAssign statement state)))))
    
;The main method to run a parse tree through my code   
(define interpret
  (lambda (file)
    (value (parser file) nullState)))
