(define (f x)
  (lambda (y)
    (+ z (* x y))))
(define y (f 3))

(define (member? elt set)
  (cond
    ((null? set) #f)
    ((eq? elt (car set)) #t)
    (else
     (member? elt (cdr set)))))
