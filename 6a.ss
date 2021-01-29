; 6a.ss
; Sybil Chen
; Assignment 6a
; 9/14/2020

; #1
(define curry2
	(lambda (p)
		(lambda (a1)
			(lambda (a2)
				(p a1 a2)))))
				
; #2
(define curried-compose
	(lambda (p1)
		(lambda (p2)
			(lambda (a)
				(p1 (p2 a))))))

; #3
(define compose	
	(lambda list-of-functions
		(if (null? (cdr list-of-functions))
			(car list-of-functions)
			(lambda (m)
				((car list-of-functions)
				((apply compose (cdr list-of-functions)) m))))))

; #4
(define make-list-c
	(lambda (a)
		(if (zero? a)
			(lambda (l) '())
			(lambda (l) (cons l ((make-list-c (- a 1)) l))))))
			
; #5
(define reverse-it
	(lambda (lst)
		(letrec ([r-helper (lambda (l1 l2)
				(if (null? l1) l2 
				(r-helper (cdr l1) (cons (car l1) l2))))])
		(r-helper lst '()))))
				
; #6
(define map-by-position
	(lambda (fn-list arg-list)
		(map (lambda (a b) (a b)) fn-list arg-list )))
		
; #7
(define empty-BST
	(lambda () '()))
	
(define empty-BST?
	(lambda (obj)
		(null? obj)))
		
(define BST-left
	(lambda (bst)
		(if (list? bst) (cadr bst) '())))
		
(define BST-right
	(lambda (bst)
		(if (list? bst) (caddr bst) '())))
		
(define BST-element
	(lambda (bst)
		(if (list? bst) (car bst) '())))
			
			
(define BST-insert
	(lambda (num bst)
		(cond 
			[(empty-BST? bst) (list num '() '())]
			[(member num bst) bst]
			[(< num (BST-element bst)) (list (BST-element bst)
			(BST-insert num (BST-left bst)) (BST-right bst))]
			[(< (BST-element bst) num) (list (BST-element bst)
			(BST-left bst) (BST-insert num (BST-right bst)))])))
		  		
(define BST-inorder
  (lambda (bst)
    (if (empty-BST? bst) '()
        (append (BST-inorder (BST-left bst)) (list (BST-element bst))
				(BST-inorder (BST-right bst))))))

(define sorted?
	(lambda (bst)
		(if (null? (cdr bst)) #t
			(if (< (BST-element bst) (BST-left bst))
				(sorted? (cdr bst)) #f))))

(define BST?
	(lambda (bst)
		(cond 
			[(null? bst) #t]
			[(not (list? bst)) #f]
			[(and (and 
					(= (length bst) 3) 
					(number? (BST-element bst))
					(list? (BST-left bst))
					(list? (BST-right bst))
					(and (BST? (BST-left bst))
						 (BST? (BST-right bst))))
			(sorted? (BST-inorder bst)))]
			[else #f])))
			
(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums) bst
      (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))

(define BST-contains?
  (lambda (bst num)
    (cond [(empty-BST? bst) #f]
          [(= (BST-element bst) num) #t]
          [(< (BST-element bst) num) (BST-contains?  (BST-right bst) num)]
		  [else (BST-contains? (BST-left bst) num)])))

(define BST-height 
	(lambda (bst)
		(if (empty-BST? bst) -1
		(+ 1 (max (BST-height (BST-left bst)) (BST-height (BST-right bst)))))))
			
; #8
(define let->application
	(lambda (l)
		(append (list 
					(list 'lambda (map car (cadr l))
					(caddr l)))
				(map cadr (cadr l)))))
					
; #9
(define let*->let
  (letrec ([l-helper (lambda (lst n)
                      (if (null? lst) n
						  (list 'let (list (car lst))
						  (l-helper (cdr lst) n) )))])
    (lambda (l)
    (l-helper (cadr l) (caddr l)))))
	
; #10
(define qsort
	(letrec ([right (lambda (pred p ls)
			(if (null? ls) '()
				(if (pred (car ls) p)
				(right pred p (cdr ls))
				(cons (car ls)(right pred p (cdr ls))))))]
			[left (lambda (pred p ls)
				(if (null? ls) '()
					(if (pred (car ls) p) 
					(cons (car ls)(left pred p (cdr ls)))
					(left pred p (cdr ls)))))])
	(lambda (pred ls)
	(if (null? ls) ls
		(append (qsort pred (left pred (car ls) (cdr ls)))
			(list(car ls))
			(qsort pred (qsort pred (right pred (car ls) (cdr ls)))))))))

; #11
(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (sort string<? (map symbol->string los)))))