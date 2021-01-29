; 7a.ss
; Assignment 7a
; Sybil Chen
; 9/17/2020

; #1					   
(define lst-helper
	(lambda (v ls indx)
        (if (null? ls)
			v
            (and (vector-set! v indx (car ls))
            (lst-helper v (cdr ls) (+ 1 indx)) v))))
			
(define vec-helper
	(lambda (v1 v2 indx)
        (if (equal? (vector-length v2) indx)
			v1
            (and (vector-set! v1 indx (vector-ref v2 indx))
            (vec-helper v1 v2 (+ 1 indx)) v1))))

(define vector-append-list
	(lambda (v ls)
	(let ([vec (make-vector (+ (length ls) (vector-length v)))])
      (vec-helper vec v 0)
      (lst-helper vec ls (vector-length v)) vec)))
	  
; #2
(define group-by-two
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[(null? (cdr ls)) (list ls)]
			[else (cons (list (car ls) (cadr ls))
					(group-by-two (cddr ls)))])))
			
; #3
(define group-by-n-helper
	(lambda (ls n)
        (if (= n 0) '()
             (cons (car ls) (group-by-n-helper (cdr ls) (- n 1))))))
			  
(define group-by-n
  (lambda (ls n)
	(cond 
		[(null? ls) '()] 
		[(< (length ls) n) (list ls)]
		[else (cons (group-by-n-helper ls n) (group-by-n (list-tail ls n) n))])))

; #4
(define bt-leaf-sum
  (lambda (T)
    (if (number? T) T
      (+ (bt-leaf-sum (cadr T)) (bt-leaf-sum (caddr T))))))
	  
(define bt-inorder-list
  (lambda (T)
      (if (number? T) '()
        (append (bt-inorder-list (cadr T)) (list (car T)) (bt-inorder-list (caddr T))))))

(define bt-max
  (lambda (T)
    (if (number? T) T
      (max (bt-max (cadr T)) (bt-max (caddr T))))))
	  
(define bt-max-helper
	(lambda (T)
		(cond
			;base case
			[(and (number? (cadr T)) (number? (caddr T)))
				(list (+ (cadr T) (caddr T)) (car T))]
			[(number? (caddr T))
				(if (> 0 (caddr T))
				(cons (+ (caddr T) (car (bt-max-helper (cadr T))))
					(cdr (bt-max-helper (cadr T))))
				(cons (+ (caddr T) (car (bt-max-helper (cadr T))))
					(list (car T))))]
			[(number? (cadr T))
				(if (> 0 (cadr T))
				(cons (+ (cadr T) (car (bt-max-helper (caddr T))))
					(cdr (bt-max-helper (caddr T))))
				(cons (+ (cadr T) (car (bt-max-helper (caddr T))))
					(list (car T))))]
			[(< (car (bt-max-helper (cadr T))) (car (bt-max-helper (caddr T))))
				(bt-max-helper (caddr T))]
			[else (bt-max-helper (cadr T))])))

(define bt-max-interior
	(lambda (T)		
		(cadr (bt-max-helper T))))