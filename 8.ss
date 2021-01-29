; 8.ss
; Assignment 8
; Sybil Chen
; 9/21/2020

; #1
(define make-stack
(lambda ()
 (let ([stk '()])
 (lambda (msg . args )
 (case msg ; Scheme's case is a similar to switch in some other languages.
 [(empty?) (null? stk)]
 [(push) (set! stk (cons (car args) stk))]
 [(pop) (let ([top (car stk)])
 (set! stk (cdr stk))
 top)]
 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))

;get help from a friend
(define make-slist-leaf-iterator
    (lambda (ls)
      (let ([stack (make-stack)])
        (letrec ([next (lambda ()
                         (if (stack 'empty?) #f
                          (let ([l (stack 'pop)])
                            (cond [(null? l) (next)]
                              [(symbol? l) l]
                              [else (begin
                                      (stack 'push (cdr l)) 
                                      (stack 'push (car l))
                                      (next))]))))])
          (begin (stack 'push ls)
            (lambda (msg . args)
              (case msg [(next) (next)])))))))

; #2
(define subst-leftmost
 (lambda (new old slist pred)
   (letrec
     ([helper (lambda (ls)
                (cond [(null? ls) '(() #f)]
                  [(list? ls)
                   (if (cadr (helper (car ls)))
                       (list (cons (car (helper (car ls))) (cdr ls)) #t)
                       (list (cons (car (helper (car ls))) (car (helper (cdr ls))))
                         (cadr (helper (cdr ls)))))]
                  ((pred ls old) (list new #t))
                 [else (list ls #f)]))])
	(car (helper slist)))))
