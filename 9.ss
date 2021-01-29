; 9.ss
; Sybil Chen
; Assignment 9

; #1
(define snlist-recur
	(lambda (base e-proc proc-list)
		(letrec ([helper (lambda (ls)
						(cond [(null? ls) base]
							[(not (pair? ls)) (e-proc ls)]
							[else (proc-list (helper (car ls))
											(helper (cdr ls)))]))])
			(lambda (ls) (helper ls)))))
; #1a
(define sn-list-sum
	(lambda (ls)
		((snlist-recur 0 + +) ls)))
		
; #1b
(define sn-list-map
	(lambda (proc ls)
		((snlist-recur '() proc cons) ls)))

; #1c
(define sn-list-paren-count
	(lambda (ls)
		((snlist-recur 2 (lambda (ls) 0) 
		(lambda (first second) (+ first second))) ls)))
; #1d
(define sn-list-reverse
	(lambda (ls)
		((snlist-recur '() (lambda (ls) ls) (lambda (first second)
		(reverse (cons first (reverse second))))) ls)))

; #1e
(define sn-list-occur
	(lambda (ca ls)
		((snlist-recur 0 (lambda (l)
					(if (equal? l ca)
					1 0)) +) ls)))

; #1f
(define sn-list-depth
	(lambda (ls)
		((snlist-recur 1 (lambda (s) 0) 
		(lambda (first second) (max (add1 first) second))) ls)))


; #2
(define bt-recur
	(lambda (n-p l-p)
	(letrec ([helper (lambda (bt)
			(cond [(number? bt) (n-p bt)]
				  [(null? bt) '()]
				  [else (l-p (car bt)
						(helper (cadr bt))
						(helper (caddr bt)))]))])
		helper)))
		
; #2a
(define (bt-sum T)
	((bt-recur + 
		(lambda 
		(bt a b) 
		(+ a b)))
		T))
	
; #2b
(define (bt-inorder T)
	((bt-recur
		(lambda (s) '())
		(lambda (bt a b)
		(append (append a (list bt)) b))) T))
		

