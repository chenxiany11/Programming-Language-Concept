; 2.ss
; Sybil Chen
; Assignment 2

; #1
; #a
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

; #b
(define choose
  (lambda (n k)
    (/ (fact n) (* (fact k) (fact (- n k))))))

; #2
(define sum-of-squares
  (lambda (lon)
      (if (equal? lon '())
          0
          (+ (* (list-ref lon 0) (list-ref lon 0))
           (sum-of-squares (cdr lon))))))

; #3
(define range
  (lambda (m n)
    (if (>= m n)
        '()
        (cons m (range (+ m 1) n)))))

; #4
(define set?
  (lambda (list)
    (cond
     [(null? list) #t]
     [(member (car list) (cdr list)) #f]
     [else (set? (cdr list))])))

; #5
(define union
  (lambda (s1 s2)
    (cond [(null? s2) (append s1)]
	  [(member (car s2) s1) (union s1 (cdr s2))]
	  [else (cons (car s2) (union s1 (cdr s2)))])))


; #6
(define cross-product
  (lambda (v1 v2)
    (list (- (*(cadr v1) (caddr v2)) (* (caddr v1) (cadr v2)))
	  (- (* (caddr v1) (car v2)) (* (car v1) (caddr v2)))
	  (- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))))))

; #7
(define parallel?
  (lambda (v1 v2)
    (and (=  (car (cross-product v1 v2)) 0)
		 (=  (cadr (cross-product v1 v2)) 0)	
		 (=  (caddr (cross-product v1 v2)) 0))))		 

(define make-vec-from-points
  (lambda (p1 p2)
    (list (- (car p2) (car p1))
      (- (cadr p2) (cadr p1))
      (- (caddr p2) (caddr p1)))))
	
; #8
(define collinear?
  (lambda (p1 p2 p3)
    (parallel? (make-vec-from-points p1 p2) (make-vec-from-points p1 p3))))
