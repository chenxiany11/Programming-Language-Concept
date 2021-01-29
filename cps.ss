;-----------------------------------------------
; CONTINUATIONS REPRESENTED BY SCHEME PROCEDURES
;-----------------------------------------------
(define apply-k
  (lambda (k v)
    (k v)))

(define make-k    
  (lambda (v) v))

(define make-cps
  (lambda (f)
    (lambda (x k)
      (apply-k k (f x)))))

(define null?-cps (make-cps null?))
(define car-cps (make-cps car))
(define cdr-cps (make-cps cdr))
(define cons-cps
  (lambda (x y k)
    (apply-k k (cons x y))))
(define eq?-cps
  (lambda (x y k)
    (apply-k k (eq? x y))))


; rewrite this in "fully CPSed" form
(define memq-cps
  (lambda (sym ls k)
    (null?-cps ls
               (lambda (v1)
                 (if v1
                     (apply-k k #f)
                     (car-cps ls
                              (lambda (v2)
                                (eq?-cps v2 sym
                                         (lambda (v3)
                                           (if v3
                                               (apply-k k #t)
                                               (cdr-cps ls
                                                        (lambda (v4)
                                                          (memq-cps sym v4 k)))))))))))))
		     
; For extra credit, rewrite this in "fully CPSed" form
(define intersection-cps
  (lambda (los1 los2 k)
    (if (null? los1) 
	(apply-k k '())
	(intersection-cps (cdr los1) los2
	    (make-k (lambda (cdr-intersection)
		      (memq-cps (car los1) los2
				(make-k (lambda (is-in?)
					  (apply-k k
						   (if is-in?
						       (cons (car los1)
							     cdr-intersection)
						       cdr-intersection)))))))))))
	


