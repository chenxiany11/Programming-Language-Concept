; 10.ss
; Assignment 10
; Sybil Chen
; 9/28/2020

; #1	  
; returns the set of all variables that occur free in e		
; having problems handle '((lambda (x) (x y)) (z (lambda (y) (z y)))))	  

(define (remove-duplicates l)
  (cond [(null? l)'()]
        [(member (car l) (cdr l))
         (remove-duplicates (cdr l))]
        [else
         (cons (car l) (remove-duplicates (cdr l)))]))
				
(define free-vars
	(lambda (e)
		(remove-duplicates (cond 
			[(symbol? e) (list e)] ; base case
			[(equal? (car e) 'lambda)
				(cond [(and (symbol? (car (cdr (cdr e))))
					  (member (car (cdr (cdr e))) (cadr e))) '()]
					  [(and (symbol? (car (cdr (cdr e))))
					  (not (member (car (cdr (cdr e))) (cadr e))))
					  (list (car (cdr (cdr e))))]
					  [(equal? (car (car (cddr e))) 'lambda)
					  (free-vars (car (cdr (cdr e))))]
					  [else (letrec ([helper (lambda (first second)
						(cond [(null? first) '()]
						[(equal? second (car first))
						(helper (car first) second)]
						[else (append (list (car first)) 
						(helper (cdr first) second))]))])
						(helper (car (cdr (cdr e))) (cadr e)))])]				 
			[(and (equal? (car e) (cadr e)) (symbol? (car e))) (list (car e))]
			[else (append (free-vars (car e)) (free-vars (cadr e)))]))))
		

; mostly same idea from free-vars		  
(define bound-vars
	(lambda (e)
		(remove-duplicates (cond
			[(symbol? e) '()] ; base case
			[(equal? (car e) 'lambda)
				(cond [(and (symbol? (car (cdr (cdr e))))
					(member (car (cdr (cdr e))) (cadr e)))
					(list (car (cdr (cdr e))))]
					[(and (symbol? (car (cdr (cdr e))))
					(not (member (car (cdr (cdr e))) (cadr e)))) '()]
					[(equal? (car (car (cddr e))) 'lambda)
						(cond [(member (caddr (car (cdr (cdr e)))) (cadr e))
								(list (caddr (car (cdr (cdr e)))))]
							  [else (bound-vars (car (cdr (cdr e))))])]
					[(equal? (caadr (car (cdr (cdr e)))) 'lambda) 
						(bound-vars (cadr (car (cdr (cdr e)))))]
					[else (letrec ([helper (lambda (first second)
						(cond [(null? first) '()]
						[(equal? second (car first))
						(helper (car first) second)]
						[(equal? second (car first))
						(append (list (car first)) (helper (cdr first) second))]
						[else (helper (car first) second)]))])
						(helper (car (cdr (cdr e))) (cadr e)))])]
			[(symbol? (car e)) (bound-vars (cadr e))]
			[else (append (bound-vars (car e)) (bound-vars (cadr e)))]))))



; #2
; code from 6a.ss
(define let->application
	(lambda (l)
		(append (list 
					(list 'lambda (map car (cadr l))
					(caddr l)))
				(map cadr (cadr l)))))
				
(define let*->let
  (letrec ([l-helper (lambda (lst n)
                      (if (null? lst) n
						  (list 'let (list (car lst))
						  (l-helper (cdr lst) n) )))])
    (lambda (l)
    (l-helper (cadr l) (caddr l)))))

(define occurs-free?
  (lambda (msg e)
    (cond
      [(null? e) #f]
      [(symbol? e) (equal? msg e)]
      [(equal? (car e) 'lambda)
            (and (not (member msg (cadr e))) (occurs-free? msg (car (cdr (cdr e)))))]
      [(equal? (car e) 'if)
            (or (or (occurs-free? msg (cadr e))
			(occurs-free? msg (car (cdr (cdr e))))) (occurs-free? msg (car (cdr (cdr (cdr e))))))]
      [(equal? (car e) 'set!)
            (occurs-free? msg (car (cdr (cdr e))))]
	  [(equal? (car e) 'let)
            (occurs-free? msg (let->application e))]
      [(equal? (car e) 'let*)
            (occurs-free? msg (let*->let e))]
      [else (ormap (lambda (a) (occurs-free? msg a)) e)])))

(define occurs-bound?
  (lambda (msg e)
    (cond
      [(null? e) #f]; base case
      [(symbol? e) #f]
      [(equal? (car e) 'lambda)
        (or (occurs-bound? msg (car (cdr (cdr e))))(and (member msg (cadr e))
                      (occurs-free? msg (car (cdr (cdr e))))))]
      [(equal? (car e) 'if)
            (or (or (occurs-bound? msg (cadr e))
			(occurs-bound? msg (car (cdr (cdr e))))) (occurs-bound? msg (car (cddr (cdr e)))))]
      [(equal? (car e) 'set!)
            (occurs-bound? msg (car (cdr (cdr e))))]
	  [(equal? (car e) 'let)
            (occurs-bound? msg (let->application e))]
      [(equal? (car e) 'let*)
            (occurs-bound? msg (let*->let e))]
      [else (ormap (lambda (a) (occurs-bound? msg a)) e)])))

; #3				
(define lexical-address-helper
  (lambda (e msg)
    (cond [(null? e) '()]
          [(symbol? e) (cons ': 
		  (letrec ([pos (lambda (msg s depth)
		  (cond [(null? s) (list 'free msg)]
          [(member msg (car s)) (list depth 
		  (letrec ([helper (lambda (first l)
		  (if (equal? first (car l)) 0
			(add1 (helper first (cdr l)))))])
		  (helper msg (car s))))]
          [else (pos msg (cdr s) (add1 depth))]))])
		  (pos e msg 0)))]
          [(null? (car e)) (lexical-address-helper (cdr e) msg)]
          [(equal? (car e) 'lambda)
              (append (list 'lambda (cadr e))
               (lexical-address-helper (cddr e) (cons (cadr e) msg)))]
          [(equal? (car e) 'if)
              (cons 'if (lexical-address-helper (cdr e) msg))]
          [(equal? (car e) 'set!)
              (append (list 'set! (cadr e)) (lexical-address-helper (cddr e) msg))]
          [(equal? (car e) 'let)
              (append (list 'let (map (lambda (first) (list (car first) (lexical-address-helper (cadr first) msg))) (cadr e)))
                (lexical-address-helper (cddr e) (cons (map car (cadr e)) msg)))]
          [else (cons (lexical-address-helper (car e) msg) 
                      (lexical-address-helper (cdr e) msg))])))
					  
(define lexical-address
  (lambda (e)
    (lexical-address-helper e '())))	
; #4
(define un-lexical-address-helper
	(lambda (e vars)
        (cond[(= (length e) 1)
            (list (un-lexical-address-helper (car e) vars))]
            [(equal? (cadr e) 'free) (caddr e)] 
			[(equal? (car e) 'if)
             (cons 'if (cons (un-lexical-address-helper (cadr e) vars)
			 (un-lexical-address-helper (cddr e) vars)))]
            [(number? (cadr e)) (list-ref (list-ref vars (cadr e)) (caddr e))]
            [(equal? (car e) 'lambda)
			(list 'lambda (cadr e) (un-lexical-address-helper (caddr e) (cons (cadr e) vars)))] 
            [(equal? (car e) 'set!)
              (cons 'set! (cons (cadr e) (un-lexical-address-helper (cddr e) vars)))]
			[(equal? (car e) 'let)
               (list 'let (map (lambda (first) (list (car first) (un-lexical-address-helper (cadr first) vars))) (cadr e))
                (un-lexical-address-helper (caddr e) (cons (map car (cadr e)) vars)))]
            [else (map (lambda (first) (un-lexical-address-helper first vars)) e)])))
			
(define un-lexical-address
    (lambda (e)
      (un-lexical-address-helper e '())))