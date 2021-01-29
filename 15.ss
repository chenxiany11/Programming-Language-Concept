; Assigenment 15
; #15.ss
; Sybil Chen
; 10/20/2020

; #1a
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    ; lambda is the "real" continuation 
  (lambda (v) v)) ; constructor here.


(define (member?-cps item L k)
	(cond [(null? L) (apply-k k #f)]
		  [(equal? item (car L))
			(apply-k k #t)]
		  [else (member?-cps item (cdr L) k)]))

(define (set?-cps L k) 
	(cond [(null? L) (apply-k k #t)]
		  [(not (pair? L)) (apply-k k #f)]
		  [else (set?-cps (cdr L) 
					(make-k (lambda (x)
						(member?-cps (car L) (cdr L)
							(make-k (lambda (y)
								(if y (apply-k k #f)
									  (apply-k k x))))))))]))

(define (set-of-cps L k)
	(if (null? L)
		(apply-k k '())
		(set-of-cps (cdr L) 
			(make-k (lambda (x)
				(member?-cps (car L) (cdr L)
					(make-k (lambda (m) 
						(apply-k k
							(if m x (cons (car L) x)))))))))))
				
(define (1st-cps L k)
	(apply-k k (car L)))
		
;(trace set-of-cps)
(define (map-cps proc L k)
	(if (null? L)
		(apply-k k '())
		(map-cps proc (cdr L) 
			(make-k (lambda (x) 
				(proc (car L)
					(make-k (lambda (result)
						(apply-k k (cons result x))))))))))

(define (domain-cps L k)
	(map-cps 1st-cps L (make-k (lambda (x) (set-of-cps x k)))))

; #1b
(define (make-cps proc)
	(lambda (arg k)
			(apply-k k (proc arg))))
			
; #1c
(define (andmap-cps pred-cps ls continuation)
	(if (null? ls)
		(apply-k continuation #t)
		(pred-cps (car ls)
			(lambda (result)
				(if result
					(andmap-cps pred-cps (cdr ls) 
						(lambda (x)
							(apply-k continuation x)))
					(apply-k continuation #f))))))
							
;(trace andmap-cps)	

; #2
;(load "chez-init.ss")
; get code from live-in-class 
(define scheme-value? (lambda (x) #t))
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define-datatype continuation continuation?
  [init-k] ; These first continuation variants need no fields.
  [list-k]
  [not-k]
  )
  
(define exp?     ; Is obj a lambda-calculus expression? This uses
  (lambda (obj)  ; our original simple definition of lc-expressions.
    (or (symbol? obj)
	(and (list? obj)
	     (or 
	      (and (= (length obj) 3)
		   (eq? (1st obj) 'lambda)
		   (list? (2nd obj))
		   (= (length (2nd obj)) 1)
		   (symbol? (caadr obj))
		   (exp? (3rd obj)))
	      (and (= (length obj) 2)
		   (exp? (1st obj))
		   (exp? (2nd obj))))))))

(define apply-k-ds
  (lambda (k v)
	(if (procedure? k) (k v)
    (cases continuation k
     [init-k () v]
     [list-k () (list v)]
     [not-k () (not v)]
     ))))	

(define memq-cps
  (lambda (sym ls k)
    (cond [(null? ls)          
	   (apply-k-ds k #f)]
	  [(eq? (car ls) sym)
	   (apply-k-ds k #t)]
	  [else (memq-cps sym (cdr ls) k)])
	 ))	 
	 
(define free-vars-cps ; convert to CPS
  (lambda (exp k)
    (cond [(symbol? exp) ;fill it in
	        (apply-k-ds k (list exp))]
	  [(eq? (car exp) 'lambda) ; fill it in
		(free-vars-cps (caddr exp)
			(make-k (lambda (x)
			(remove-cps (car (cadr exp)) x k))))
	   ]
	  [else ; fill it in
	   (free-vars-cps (car exp)
			(make-k (lambda (x)
				(free-vars-cps (cadr exp)
					(make-k (lambda (y)
						(union-cps x y k)))))))
	   ])))		
;(trace free-vars-cps)
		
(define union-cps ; assumes that both arguments are sets of symbols
  (lambda (s1 s2 k)
    (if (null? s1) ; fill it in
		(apply-k-ds k s2)
		(memq-cps (car s1) s2
			(make-k (lambda (x) 
				(union-cps (cdr s1) s2
					(make-k (lambda (y)
						(if x (union-cps (cdr s1) s2 k)
							  (apply-k-ds k (cons (car s1) y)))))))))
)))

(define remove-cps ; removes the first occurrence of element in ls
  (lambda (element ls k)
    (cond [(null? ls) (apply-k-ds k '())]
		  [(equal? element (car ls)) (apply-k-ds k (cdr ls))]
		  [else (remove-cps element (cdr ls)
			(make-k (lambda (x)
				(apply-k-ds k (if (eq? element (car ls))
								  x
								  (cons (car ls) x))))))])))
	
		

						