(load "verySimpleParser.scm")


(define lookup
  (lambda (var environment)
    (cond
      ((number? var) var) 
      ((eq? var 'true) var)
      ((eq? var 'false) var)
      ((null? (car environment)) null)
      ((eq? var (caar environment)) (caadr environment))
      (else (lookup var (cons (cdr (car environment)) (cons (cdr (cadr environment))'())))))))

(define add
  (lambda (var val enviornment)
    (cons (cons var (car enviornment)) (cons (cons val (cadr enviornment)) '()))))

(define myremove
  (lambda (var enviornment)
    (cond
      ((null? (car enviornment)) '(()()))
      ((eq? (caar enviornment) var) (myremove var (cons (cdar enviornment) (cons (cdadr enviornment) '()))))
      (else (cons (cdr enviornment) (cons (cdadr enviornment) '()))))))

; A MValue function that uses abstraction to allow expressions in prefix,
; postfix, or infix format
; Call as (eval-expression '(3 + 4) '() infix)
(define eval-expression
  (lambda (expression environment form)
    ((lambda (operator left-operand right-operand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((or (eq? 'true operator) (eq? 'true expression)) #t)
         ((or (eq? 'false operator) (eq? 'false expression)) #f)
         ((eq? '+ (operator expression)) (+ (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((and (eq? '- (operator expression)) (null? (cdr (cdr expression)))) (- (eval-expression (cadr expression) environment form)))
         ((eq? '- (operator expression)) (- (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '* (operator expression)) (* (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '/ (operator expression)) (quotient (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((eq? '% (operator expression)) (modulo (eval-expression (left-operand expression) environment form) (eval-expression (right-operand expression) environment form)))
         ((number? (car expression)) expression)
         ((number? (lookup expression environment)) (lookup expression environment))
         (else (eval-expression-bool expression environment form))))
     (get-operator form) (get-left-operand form) (get-right-operand form))))


;==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !
(define eval-expression-bool
  (lambda (expression environment form)
    ((lambda (operator left-operand right-operand)
       (cond
         ((not (list? expression)) (lookup expression environment))
         ((eq? 'true (car expression)) #t)
         ((eq? 'false (car expression)) #f)
         ((eq? '&& (operator expression)) (and (eval-expression-bool (left-operand expression) environment form) (eval-expression-bool (right-operand expression) environment form)))
         ((eq? '! (operator expression))  (not (eval-expression (cadr expression) environment form)))
         ((eq? '|| (operator expression)) (or (eval-expression-bool (left-operand expression) environment form) (eval-expression-bool (right-operand expression) environment form)))
         ((eq? '== (operator expression)) (equal? (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form)))
         ((eq? '!= (operator expression)) (not (equal? (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form))))
         ((eq? '>= (operator expression)) (>= (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form)))
         ((eq? '> (operator expression)) (> (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form)))
         ((eq? '<= (operator expression)) (<= (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form)))
         ((eq? '< (operator expression)) (< (eval-expression-bool (eval-expression (left-operand expression) environment form) environment form) (eval-expression-bool (eval-expression (right-operand expression) environment form) environment form)))
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

(define prefix
  (lambda ()
    (list car cadr caddr)))

;(define M_state
;  (lambda (statement enviornment)
;    (cond
;      ((and (null? (caddr (car statement))) (eq? 'var (caar statement))) (M_state (cdr statement) (add (cadar statement) 'und enviornment)))
;      ((and (not (null? (cddr (car statement)))) (eq? 'var (caar statement))) (M_state (cdr statement) (add (cadar statement) (M_value (caddar statement) enviornment) enviornment)))
;      ((eq? '= (caar statement)) (M_state (cdr statement) (add (cadar statement) (car (M_value (cddar statement) enviornment)) (myremove (cadar statement) enviornment))))
;      ((and (eq? 'if (caar statement)) (eq? #t (M_boolean (cadar statement) enviornment))) (M_state (caddar statement) enviornment))
;      ((and (eq? 'if (caar statement)) (eq? #f (M_boolean (cadar statement) enviornment))) (M_state (car (cdddar statment)) enviornment))
;      ((eq? 'return (caar statement)) (return (M_value (cadar statement) enviornment) enviornment))
;      (else (error 'undefined-expression)))))

(define M_state
  (lambda (statement enviornment)
    (cond
      ((or (eq? 'var (car statement)) (eq? '= (car statement))) (M_state_cast statement enviornment))
      ((eq? 'if (car statement)) (M_state_if statement enviornment))
      ((eq? 'return (car statement)) (return (M_value (cadr statement) enviornment) enviornment))
      (else (error 'undefined-expression)))))
                     

(define M_state_if
  (lambda (statement enviornment)
    (cond
      ((eq? #t (M_boolean (cadr statement) enviornment)) (M_state (caddr statement) enviornment))
      ((null? (cdddr statement)) enviornment)
      (else (M_state (cadddr statement) enviornment))))) 
     
(define M_state_cast
  (lambda (statement enviornment)
    (cond
      ((and (eq? '= (car statement)) (or (null? (lookup (cadr statement) enviornment)) (and (eq? 'und (lookup (cadr statement) enviornment)) (eq? 'und (lookup (caddr statement) enviornment))))) (error 'ERROR))
      ((and (null? (cddr statement)) (eq? 'var (car statement))) (add (cadr statement) 'und enviornment))
      ((and (not (null? (cddr statement))) (eq? 'var (car statement))) (add (cadr statement) (M_value (caddr statement) enviornment) enviornment))
      ((eq? '= (car statement))  (add (cadr statement) (M_value (caddr statement) enviornment) (myremove (cadr statement) enviornment)))
      (else (error 'value-not-defined)))))

(define return
  (lambda (val enviornment)
    (cond
      ((number? val) (add 'return val enviornment))
      ((or (eq? val #t) (eq? val 'true)) (add 'return 'true enviornment))
      ((or (eq? val #f) (eq? val 'false)) (add 'return 'false enviornment))
      ((null? (lookup val enviornment)) 'und)
      (else (lookup val enviornment)))))

; ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, 
(define M_boolean
  (lambda (expression enviornment)
    (eval-expression-bool expression enviornment prefix)))

(define M_value
  (lambda (expression enviornment)
    (eval-expression expression enviornment prefix)))

(define interpret
  (lambda (fileName)
    (M_state_list (parser fileName) '(()()))))

(define M_state_list
  (lambda (list enviornment)
    (cond
      ((null? list) enviornment)
      (else (M_state_list (cdr list) (M_state (car list) enviornment))))))
