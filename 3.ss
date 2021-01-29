; 3.ss
; Sybil Chen
; Assignment 3
; 9/8/2020

; #1
(define intersection
  (lambda (s1 s2)
    (cond
      [(null? s2) '()]
      [(member (car s2) s1)
       (cons (car s2) (intersection s1 (cdr s2)))]
      [else (intersection s1 (cdr s2))])))

; #2
(define subset?
  (lambda (s1 s2)
    (cond
      [(null? s1) #t]
      [(member (car s1) s2)
       (and #t (subset? (cdr s1) s2))]
      [else #f])))


(define set?
  (lambda (list)
    (cond
     [(null? list) #t]
     [(member (car list) (cdr list)) #f]
     [else (set? (cdr list))])))

(define check-list
	(lambda (obj)
		(if (null? obj) #t
			(if (list? (car obj)) (check-list (cdr obj))
			#f))))
; #3
(define relation? 
  (lambda (obj)
    (cond
		[(null? obj) #t]
		[(not (list? obj)) #f]
		[(not (set? obj)) #f]
		[(not (check-list obj)) #f]
		[(not (null? (cddar obj))) #f]
		[else (relation? (cdr obj))])))
		
; #4
(define domain
  (lambda (r)
	(cond
		[(null? r) '()]
		[ (member (caar r) (domain (cdr r))) (domain (cdr r))]
		[else (cons (caar r) (domain (cdr r)))])))
		
; #5
(define range
  (lambda (r)
	(cond
		[(null? r) '()]
		[ (member (cadar r) (range (cdr r))) (range (cdr r))]
		[else (cons (cadar r) (range (cdr r)))])))

(define union
  (lambda (s1 s2)
    (cond [(null? s2) (append s1)]
	  [(member (car s2) s1) (union s1 (cdr s2))]
	  [else (cons (car s2) (union s1 (cdr s2)))])))
	  
(define reflexive-helper
	(lambda (r l)
		(cond
			[(null? l) #t]
			[(member (list (car l) (car l)) r) 
				(reflexive-helper r (cdr l))]
			[else #f])))
			
(define reflexive?
	(lambda (r)
	(reflexive-helper r (union (domain r) (range r)))))
		

; #6
(define multi-set?
  (lambda (obj)
	(cond 
	  [(not (relation? obj)) #f]
      [(member #f (map check-number obj)) #f]
      [else (set? (map car obj))])))

(define check-number
  (lambda (ls)
    (and (symbol? (car ls))
		(and (integer? (cadr ls)) (positive? (cadr ls))))))

; #7
(define ms-size
  (lambda (ms)
    (apply + (map cadr ms))))
	
	
; #8
(define last
  (lambda (ls)
    (cond 
		[(null? (cdr ls)) (car ls)]
		[else (last (cdr ls))])))

; #9
(define all-but-last
  (lambda (lst)
    (cond 
		[(null? (cdr lst)) '()]
		[else (append (list (car lst)) (all-but-last (cdr lst)))])))