; 5.ss
; Assignment 5
; Sybil Chen

; #1
(define d-sort
	(lambda (l1 l2)
		(< (car l1) (car l2))))
		
(define mil-helper
	(lambda (ls)
		(cond
			[(equal? (length ls) 1) ls]
			[(< (cadar ls) (caadr ls)) 
			(cons (car ls) (mil-helper (cdr ls)))] 
			[else (mil-helper(cons (list (car (car ls))
								(max (cadar ls)(cadadr ls)))
								(cddr ls)))])))
							
(define minimize-interval-list
  (lambda (ls)
	(mil-helper (list-sort d-sort ls))))

; #2
(define exists? 
	(lambda (pred ls)
		(cond
			[(null? ls) #f]
			[else (or (pred (car ls)) (exists? pred (cdr ls)))])))
	
; #3
(define product-helper
	(lambda (c s2)
		(if (null? (cdr s2)) 
			(list (list c (car s2)))
			(cons (list c (car s2)) (product-helper c (cdr s2))))))
			
(define product
  (lambda (set1 set2)
	(cond 
		[(or (null? set1) (null? set2)) '()]
		[(null? set1) (product-helper (car set1) set2)]
		[else (append (product-helper (car set1) set2) (product (cdr set1) set2))])))
	  
; #4
(define replace
	(lambda (old new ls)
		(cond 
			[(null? ls) '()]
			[(= old (car ls))
			(cons new (replace old new (cdr ls)))]
			[else (cons (car ls) (replace old new (cdr ls)) )])))

; #5
(define remove-last
	(lambda (element ls)
		(cond 
			[(and (member element ls) (member element (cdr ls)))
			(cons (car ls) (remove-last element (cdr ls)))]
			[(and (member element ls) (not (member element (cdr ls))))
			(cdr ls)]
			[else ls])))
			
				