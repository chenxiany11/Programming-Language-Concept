; 7a.ss
; Assignment 7a
; Sybil Chen
; 9/17/2020

; #5a
(define slist-map
	(lambda (proc slist)
  (let slist-map ([slist slist])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist)) (cons (proc (car slist)) (slist-map (cdr slist)))]
	  [else (cons (slist-map (car slist)) (slist-map (cdr slist)))]))))  
  
 ; #5b
(define slist-reverse 
	(lambda (slist)
		(cond 
			[(null? slist) '()]
			[(null? (car slist)) (append (slist-reverse (cdr slist)) (list '()))]
			[(list? (car slist)) (append (slist-reverse (cdr slist))
										(list (slist-reverse (car slist))))]
			[else (append (slist-reverse (cdr slist))
				(list (car slist)))])))
 
; #5c
(define slist-paren-count
	(lambda (slist)
		(if (null? slist) 2
			(if (list? (car slist))
				(+ (slist-paren-count (car slist))(slist-paren-count (cdr slist)))
				(slist-paren-count (cdr slist))))))

; #5d
(define slist-depth 
	(lambda slist
	(let depth-helper ([s1 slist] [dp 0])
		(cond 
			[(null? s1) dp]
			[(symbol? (car s1)) (max dp (depth-helper (cdr s1) dp))]
			[else (max (depth-helper (car s1) (add1 dp))
						(depth-helper (cdr s1) dp))]))))

; #5e
(define slist-symbols-at-depth
	(lambda (slist d)
		(let symbol-helper ([slist slist] [dp 1])
			(cond [(null? slist) '()]
				  [(= d dp)
				  	  (if (symbol? (car slist))
				  	  	  (cons (car slist) (symbol-helper (cdr slist) dp))
				  	  	  (symbol-helper (cdr slist) dp))]
				  [else 
				  	   (if (symbol? (car slist))
				  	   	   (symbol-helper (cdr slist) dp)
				  	   	   (append (symbol-helper (car slist) (add1 dp))
				  	   	   		   (symbol-helper (cdr slist) dp)))]))))
; #6
(define path-to-helper
	(lambda (s sym)
		(cond [(null? s) '(#f)]
			  [(equal? sym (car s)) '(car)]
			  [else 
					(if (and (list? (car s)) (not (null? (car s)))) 
						(append '(car) (path-to-helper (car s) sym))
						(if (path-to-helper (cdr s) sym)
							(append '(cdr) (path-to-helper (cdr s) sym))
							'(#f)))])))
(define path-to
	(lambda (s sym)
		(if (equal? #f (car (reverse (path-to-helper s sym))))
			#f
			(path-to-helper s sym))))
									
; #7
(define compose
 (case-lambda
 [() (lambda (x) x)]
 [(first . rest)
 (let ([composed-rest (apply compose rest)])
 (lambda (x) (first (composed-rest x))))]))
 
(define (make-c...r s)
     (apply compose (make-c...r-helper (string->list s))))

(define make-c...r-helper 
	(lambda (ls)
      (cond
        [(null? ls) '()]
        [(equal? (car ls) #\a) (cons car (make-c...r-helper  (cdr ls)))]
        [(equal? (car ls) #\d) (cons cdr (make-c...r-helper  (cdr ls)))])))
						   