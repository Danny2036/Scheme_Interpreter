;Daniel McKinnon
;EECS 345: PLC
;Programming Assignment 2

;multiplyBy: takes a number and a list of numbers and returns a list that is the input list with each element multiplied by the input number.
(define multiplyBy-cps
  (lambda (a l return)
    (cond
      ((null? l) (return '()))
      (else (multiplyBy-cps a (cdr l) (lambda (v) (return (cons (* a (car l)) v))))))))

;crossMultiply:  takes two lists of numbers, each list represents a vector. Returns the cross product of the two vectors
(define crossMultiply-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return '()))
      (else (crossMultiply-cps (cdr l1) l2 (lambda (v1) (multiplyBy-cps (car l1) l2 (lambda (v2) (return (cons v2 v1))))))))))

;maxNumber: takes a list of numbers that contains at least one number and returns the largest number in the lis
(define maxNumber-cps
  (lambda (l return)
    (cond
      ((null? l) (return 0))
      ((eq? 1 (length l)) (return (car l)))
      (else (maxNumber-cps (cdr l) (lambda (v) (return (if (> (car l) v) (car l) v))))))))

;partialsums*: Calculates the sum of a list and every sublist
(define partialSums*-cps
  (lambda (l return)
    (cond
      ((null? l)  (return '(0)))
      ((list? (car l)) (partialSums*-cps (car l) (lambda (v1) (partialSums*-cps (cdr l) (lambda (v2) (return (cons (+ (car v1) (car v2)) (cons  v1 (cdr v2)))))))))
      ((number? (car l)) (partialSums*-cps (cdr l) (lambda (v) (return (cons (+ (car l) (car v)) (cdr v))))))
      (else (partialSums*-cps (cdr l) return)))))

;trimatoms takes a list, possibly containing sublists, and a list of atoms. It returns the list of atoms with the first k atoms of the list removed where k is the number of non-null atoms in the first list
(define trimAtoms-cps
  (lambda (l1 l2 return)
    (cond
      ((null? l1) (return l2))
      ((list? (car l1)) (trimAtoms-cps (car l1) l2 (lambda (v1) (trimAtoms-cps (cdr l1) v1 return))))
      (else (trimAtoms-cps (cdr l1) (cdr l2) return)))))

;exchange: Swaps each atoms with their respective atoms from the second list
(define exchange-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return'()))
      ((list? (car l1)) (exchange-cps (car l1) l2 (lambda (v1) (exchange-cps (cdr l1) (trimAtoms-cps (car l1) l2 return) (lambda (v2) (return (cons v1 v2)))))))
      (else (exchange-cps (cdr l1) (cdr l2) (lambda (v) (return (cons (car l2) v))))))))

;removesubsequence*: removes the first occurence of list 1 from list 2 regardless of the number of nested lists in list 2
(define removeSubsequence*-cps
  (lambda (l1 l2 return)
    (cond
      ((or (null? l1) (null? l2)) (return l1 l2))
      ((list? (car l2)) (removeSubsequence*-cps l1 (car l2) (lambda (v1 v2) (removeSubsequence*-cps v1 (cdr l2) (lambda (v3 v4) (return v3 (cons v2 v4)))))))
      (else (removeSubsequence*-cps l1 (cdr l2) (lambda (v1 v2) (return v1 (cons (car l2) v2))))))))

;split-cps: places all the odd indices of the list and puts them in one list and the even indicies go into another
(define split
  (lambda (l return)
    (cond
      ((null? l) (return '() '()))
      (else (split (cdr l) (lambda (v1 v2) (return (cons (car l) v2) v1)))))))

;suffix: takes an atom and a list. Returns only the portion of the list after the last occurence of the atom
(define suffix
  (lambda (a l)
    (letrec
        ((loop (lambda (a l return)
                 (cond
                   ((null? l) (return '()))
                   ((eq? a (car l)) (loop a (cdr l) (lambda (v) v)))
                   (else (loop a (cdr l) (lambda (v) (return (cons (car l) v))))))))) 
      (loop a l (lambda (v) v)))))

;sufix2: re: suffix
(define suffix2
  (lambda (a l)
      (call/cc
       (lambda (break)
        (letrec
        ((loop (lambda (a l return)
                 (cond
                   ((null? l) (return '()))
                   ((eq? a (car l)) (break (suffix2 a (cdr l))))
                   (else (loop a (cdr l) (lambda (v) (return (cons (car l) v))))))))) 
          (loop a l (lambda (v) v))))))) 

; (flatten-cps (car l) (lambda (v1) (flatten-cps (cdr l) (lambda (v2) (myappend-cps v1 v2 return))))))

(define ret (lambda (v) v))