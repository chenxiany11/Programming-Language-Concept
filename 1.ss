;   1.ss
;   Assignment_01
;   Sybil Chen


; #1
(define interval-contains?
  (lambda (interval number)
    (and (<= (list-ref interval 0) number)
             (>= (list-ref interval 1) number))))


; #2
(define interval-intersects?
  (lambda (i1 i2)
     (<= (max (list-ref i1 0) (list-ref i2 0))
     (min (list-ref i1 1) (list-ref i2 1)))))

; #3
(define interval-union
  (lambda (i1 i2)
    (if (equal? (interval-intersects? i1 i2)  #f)
        (list i1 i2)
        (list (list (min (list-ref i1 0) (list-ref i2 0))
                (max (list-ref i1 1) (list-ref i2 1)))))))

; #4
(define first 
  (lambda (l)
    (car l)))

(define second
  (lambda (l)
    (cadr l)))

(define third 
  (lambda (l)
    (caddr l)))

; #5
(define make-vec-from-points
  (lambda (p1 p2)
    (list (- (car p2) (car p1))
      (- (cadr p2) (cadr p1))
      (- (caddr p2) (caddr p1)))))

; #6
(define dot-product 
  (lambda (v1 v2)
    (+ (* (car v1) (car v2))
     (* (cadr v1) (cadr v2))
     (* (caddr v1) (caddr v2)))))

; #7
(define vector-magnitude
  (lambda (v)
    (sqrt (+ (* (car v) (car v))
           (* (cadr v) (cadr v))
           (* (caddr v) (caddr v))))))

; #8
(define distance
  (lambda (p1 p2)
    (let ([v (make-vec-from-points p1 p2)])
     (vector-magnitude v))))