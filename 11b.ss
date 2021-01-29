;Sybil Chen, Jacob Hendrich, Myon McGee
(load "chez-init.ss")
;parser
(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (id symbol?)
   (body expression?)]
  [app-exp
   (rator expression?)
   (rand expression?)]
  [lit-exp
   (id scheme-value?)]
  [no-parens-lambda-exp
   (id symbol?)
   (body (list-of expression?))]
  [improper-lambda-exp
   (ids expression?)
   (body (list-of expression?))]
  [let-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?))]
  [let*-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?))]
  [letrec-exp
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?))]
  [named-let-exp
   (name expression?)
   (ids (list-of expression?))
   (values (list-of expression?))
   (body (list-of expression?))]
  [set!-exp
   (id symbol?)
   (body expression?)]
  [if-exp
   (test expression?)
   (true expression?)
   (false expression?)]
  [no-else-if-exp
   (test expression?)
   (true expression?)])

(define scheme-value?
  (lambda (val)
    #t))

(define custom-number
  (lambda (ls)
    (cond [(null? ls) #t]
	  [(number? (car ls)) #f]
	  [else (custom-number (cdr ls))])))

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define (proper-list? x)
  (cond ((null? x) #t)
        ((pair? x) (proper-list? (cdr x)))
        (else #f)))
		
(define parse-exp
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(boolean? datum) (lit-exp datum)]
     [(string? datum) (lit-exp datum)]
     [(vector? datum) (lit-exp datum)]
     [(number? datum) (lit-exp datum)]
     [(not (proper-list? datum))
      (eopl:error 'parse-exp "Error in parse-exp: Improper list in ~s" datum)]
     [(pair? datum)
      (cond
       [(not (proper-list? datum)) (lit-exp datum)]
       
       [(eqv? (car datum) 'quote)
	(if (equal? (length datum) 2)
	    (lit-exp datum)
	    (eopl:error 'parse-exp "Error in parse-exp: Invalids syntax ~s" datum))]
       
       [(eqv? (car datum) 'lambda)
	(if (> (length datum) 2)
	    (if (symbol? (cadr datum))
	        (no-parens-lambda-exp (cadr datum) (map parse-exp (cddr datum)))
	        (if (and (proper-list? (cadr datum))
	                 (and (map char? (cadr datum)))
	                 (not (list-contains-multiples? (cadr datum)))
	                 (custom-number (cadr datum)))
	            (lambda-exp (map parse-exp (cadr datum)) (map parse-exp (cddr datum)))
	            (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)))
	    (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum))]
       
       [(eqv? (car datum) 'let)
	(cond
	 [(and (> (length datum) 2) (proper-list? (cadr datum))
	       (andmap proper-list? (cadr datum))
	       (andmap (lambda (ls) (equal? (length ls) 2)) (cadr datum))
	       (andmap (lambda (ls) (symbol? (car ls))) (cadr datum))
	       (not (list-contains-multiples? (map car (cadr datum)))))
	  (let-exp (map parse-exp (map car (cadr datum)))
		   (map parse-exp (map cadr (cadr datum)))
		   (map parse-exp (cddr datum)))]
	 [else (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)])]
       
       [(eqv? (car datum) 'let*)
	(if (and (> (length datum) 2) (proper-list? (cadr datum))
	         (andmap proper-list? (cadr datum))
	         (andmap (lambda (ls) (equal? (length ls) 2)) (cadr datum))
	         (andmap (lambda (ls) (symbol? (car ls))) (cadr datum))
	         (not (list-contains-multiples? (map car (cadr datum)))))
	    (let*-exp (map parse-exp (map car (cadr datum)))
	              (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
	    (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum))]
       
       [(eqv? (car datum) 'letrec)
	(if (letrec-checker datum)
	    (letrec-exp (map parse-exp (map car (cadr datum)))
	                (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))
	    (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum))]

       ;set!
	   [(eqv? (car datum) 'set!)
	   (if (eqv? 3 (length datum))
	   (set!-exp (parse-exp (2nd datum))
			(parse-exp (3rd datum)))
		(eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum))]
								
	   
       ;if
       [(eqv? (car datum) 'if)
	   (cond [(eqv? 4 (length datum))
		(no-else-if-exp (parse-exp (2nd datum))
			(parse-exp (3rd datum)) (parse-exp (cadddr datum)))]
			[(eqv? 3 (length datum)) 
			(if (parse-exp (2nd datum))
			(parse-exp (3rd datum)) (parse-exp (4th datum)))]
		(eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum))]
       [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
     [else (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax ~s" datum)])))

(define letrec-checker
  (lambda (datum)
    (cond
     [(null? (cadr datum))
      (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum)]
     [(null? (cddr datum))
      (eopl:error 'parse-exp "Error in parse-exp: Incorrect length in ~s" datum)]
     [(not (list? (cdr datum)))
      (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax, declarations must be lists in ~s" datum)]
     [(null? (caadr datum))
      (eopl:error 'parse-exp "Error in parse-exp: No declarations made in ~s" datum)]
     [(not (andmap (lambda (ls) (equal? (length ls) 2)) (cadr datum)))
      (eopl:error 'parse-exp "Error in parse-exp: not all length 2 in ~s" datum)]
     [(not (andmap symbol? (map car (cadr datum))))
      (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax, declarations must be symbols in ~s" datum)]
     [(list-contains-multiples? (map car (cadr datum)))
      (eopl:error 'parse-exp "Error in parse-exp: Invalid syntax, declarations must not repeat symbols in ~s"
		  datum)]
     [else #t])))

(define list-contains-multiples?
  (lambda (ls)
    (cond
     [(null? ls) #f]
     [(member (car ls) (cdr ls)) #t]
     [else (list-contains-multiples? (cdr ls))])))


(define unparse-exp
    (lambda (e)
        (cases expression e
            [var-exp (id) id]
            [lit-exp (id) id]
            [var-exp (v) v]
            [lambda-exp (id body)
			(apply list 'lambda id (map unparse-exp body))]
            [let-exp (id e)
			(apply list 'let (map unparse-exp id) (map unparse-exp e))]
            [let*-exp (id e)
			(apply list 'let* (map unparse-exp id) (map unparse-exp e))]
            [letrec-exp (id e) 
			(apply list 'letrec (map unparse-exp id) (map unparse-exp e))]
			 [set!-exp (id e)
			(apply list 'set! id (unparse-exp e))]
            [no-else-if-exp (cond then else)
			(list 'if (unparse-exp cond) (unparse-exp then) (unparse-exp else))]
			[if-exp (test true false) 
			(list 'if (unparse-exp test) (unparse-exp true) (unparse-exp false))]
			; else: app-exp
            [else (rator rand) (cons (unparse-exp rator) (map unparse-exp rand))])))

;An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda (x)
   (cases expression x
     [var-exp (id) #t]
     [else #f])))
(var-exp? (var-exp 'a))
(var-exp? (app-exp (var-exp 'a) (var-exp 'b)))
