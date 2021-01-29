; 4.ss
; Sybil Chen
; 9/10/20
; Assignment 4

; #1
(define matrix-ref
	(lambda (m row col)
		(list-ref (list-ref m row) col)))

; #2
(define matrix-helper
	(lambda (obj len)
		(cond 
			[(null? obj) #t]
			[(member #f (map number? (car obj))) #f]
			[(= len (length(car obj))) (matrix-helper (cdr obj) len)]
			[else #f])))
			

(define matrix?
	(lambda (obj)
		(cond 
			[(not (list? obj)) #f]	
			[(member #f (map list? obj)) #f]
			[(null? (car obj)) #f]
			[else (matrix-helper obj (length (car obj)))])))
			
; #3
(define matrix-transpose
	(lambda (m)
		(cond 
			[(null? (car m)) '()]
			[else (cons (map car m) (matrix-transpose (map cdr m)))])))

; #4
(define filter-in 
	(lambda (pred? lst)
		(cond 
			[(null? lst) '()]
			[(pred? (car lst)) (cons (car lst) (filter-in pred? (cdr lst)))]
			[else (filter-in pred? (cdr lst))])))
			
; #5
(define invert
	(lambda (lst)
		(if (null? lst) '()
			(cons (list (cadar lst) (caar lst))
				(invert (cdr lst))))))

; #6
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

(define choose
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact (- n k))))))
	
(define pascal-triangle-helper
	(lambda (r i)
		(cond
			[(= r i) '(1)]
			[(cons (choose r i) (pascal-triangle-helper r (+ 1 i)))])))

(define pascal-triangle
	(lambda (n)
		(cond 
			[(< n 0) '()]
			[(= 0 n) '((1))]
			[else (cons (pascal-triangle-helper n 0) 
				(pascal-triangle (- n 1)))])))