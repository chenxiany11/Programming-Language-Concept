; 11.ss
; Sybil Chen 
; 10/1/2020

; #1a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((v val) ...) exp1 exp2 ...)
		 ((lambda (v ...) exp1 exp2 ...) val ...)]
		 [(_ name ([v k] ...) exp1)
			(letrec ([name 
					(lambda (v ...) exp1)]) (name k ...))]))
			
; #1b
(define-syntax my-or
    (syntax-rules ()
        [(_) #f] [(_ e) e] [(_ exp1 exp2 ...)
        (let [(temp exp1)]
            (if temp temp (my-or exp2 ...)))]))

; #c
(define-syntax +=
	(syntax-rules ()
		[(_ a b)
		(begin (set! a (+ a b)) a)]))

; #1d
(define-syntax return-first
	(syntax-rules ()
		[(_ exp1) exp1]
		[(_ exp1 exp2 ...) (let ([temp exp1])
			exp2 ... temp)]))
			
; #2
;; Binary trees using define-datatype
;(load "chez-init.ss") ; chez-init.ss should be in the same folder as this code.
;; from EoPL, page 50
(define-datatype bintree bintree?
 (leaf-node
 (num integer?))
 (interior-node
 (key symbol?)
 (left-tree bintree?)
 (right-tree bintree?)))
 
(define bintree-to-list
  (lambda (tree)
  (cases bintree tree
    (leaf-node (num) (list 'leaf-node num))
    (interior-node (key left right)
         (list 'interior-node key
            (bintree-to-list left)
            (bintree-to-list right))))))
						 
; #3
(define max-interior
  (lambda (node)
	(letrec ([max-interior-helper (lambda (node)
	(cases bintree node
    (leaf-node (num) (list '() 'leaf-node num))
    (interior-node (key left right)
      (let* ([l (max-interior-helper left)]
			[r (max-interior-helper right)])
	   (let ([sum (+ (car (cddr l)) (car (cddr r)))])
       (cond [(and (or (null? (car l))
                (<= (cadr l) sum))
                 (or (null? (car r))(<= (cadr r) sum )))
                (list key sum sum)]
             [(and (not (null? (car l)))
                 (or (null? (car r))
                      (<= (cadr r) (cadr l) )))
                 (list (car l) (cadr l) sum)]
             [else (list (car r) (cadr r) sum)]))))))])
	(car (max-interior-helper node)))))
			   
