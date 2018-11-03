"Lab 7"
"Nikhil Paranjape"
"2018-10-26"

;Problem 1
(display "\nProblem 1\n")
;Takes a list L and removes a value X

(define (remove-all x L)
  (cond ((null? L) L)
        ((list? (car L))
         (cons (remove-all x (car L)) (remove-all x (cdr L))))
        ((equal? x (car L)) (remove-all x (cdr L)))
        (else (cons (car L) (remove-all x (cdr L))))))

(display "(remove-all 'a '(a b a c d e))           = ")
(remove-all 'a '(a b a c d e))
(display "(remove-all 'z '(a b a c))               = ")
(remove-all 'z '(a b a c))

(display "(remove-all 'a '(a (a b c) b a ((a)) c)) = ")
(remove-all 'a '(a (a b c) b a ((a)) c))

;Problem 2a
(display "\nProblem 2a\n")
;The function dot-prod takes two lists of numbers as its inputs
; and returns the dot product of those two lists.

(define (dot-prod L1 L2)
  (cond ((null? L1) 0)
        (else
         (+ (* (car L1) (car L2))
            (dot-prod (cdr L1) (cdr L2))))))

(display "(dot-prod '(1 2 3) '(4 5 6))          = ")
(dot-prod '(1 2 3) '(4 5 6))

(display "(dot-prod '(2 4 4) '(3 5 7))          = ")
(dot-prod '(2 4 4) '(3 5 7))

(display "(dot-prod '(3 4 5) '(6 7 8))          = ")
(dot-prod '(3 4 5) '(6 7 8))

;Problem 2b
(display "\nProblem 2b\n")

(define (dot-prod-with-map L1 L2)
  (apply + (map * L1 L2)))

(display "(dot-prod-with-map '(1 2 3) '(4 5 6)) = ")
(dot-prod-with-map '(1 2 3) '(4 5 6))

(display "(dot-prod-with-map '(3 4 5) '(6 7 8)) = ")
(dot-prod-with-map '(3 4 5) '(6 7 8))

(display "(dot-prod-with-map '(1 3) '(1 5 3))   = ")
(dot-prod-with-map '(1 3) '(1 5 3))

;Problem 3
(display "\nProblem 3\n")
;Takes two sets and returns the union of those two sets (which is a set)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define union-set 
  (lambda (s1 s2)
    (cond ((null? s1) s2)
          ((member (car s1) s2)
           (union-set (cdr s1) s2))
          (else (cons (car s1)
                      (union-set (cdr s1) s2))))))

(display "(union-set '(a b a c) '(a b c))               = ")
(union-set '(a b a c) '(a b c))

(display "(union-set '(a b a c e f) '(a b c c e d))     = ")
(union-set '(a b a c e f) '(a b c c e d))

(display "(union-set '(a b a c e d f) '(a d b c c e d)) = ")
(union-set '(a b a c e d f) '(a d b c c e d))

;Problem 4
(display "\nProblem  4\n")
;Takes a list and returns #f if no two of its members are equal. #t otherwise

(define (has-duplicates? lst) 
  (cond ((null? lst) #f)
        ((member (car lst) (cdr lst)) #t)
        (else (has-duplicates? (cdr lst)))))

(display "(has-duplicates? '(a b c a d e c)) = ")
(has-duplicates? '(a b c a d e c))

(display "(has-duplicates? '(b c a d e c))   = ")
(has-duplicates? '(b c a d e c))

(display "(has-duplicates? '(a b c d e f))   = ")
(has-duplicates? '(a b c d e f))

;Problem 5
(display "\nProblem 5\n")
;Returns the number of zeroes in a list

(define (num-zeroes lst)
  (define (helper lst s)
    (cond ((null? lst) s)
          ((list? (car lst)) (+ s                       
                                (helper (car lst) 0)         
                                (helper (cdr lst) 0)))       
          ((eq? (car lst) 0) (helper (cdr lst) (+ 1 s))) 
          (else
           (helper (cdr lst) s))))
  (helper lst 0))

(display "(num-zeroes '(0 0 (1 (1 (0 0) 0)) (1 0 0)))            = ")
(num-zeroes '(0 0 (1 (1 (0 0) 0)) (1 0 0)))

(display "(num-zeroes '(0 0 0 (1 (1 (0 ( 0 ( 0))0) 0)) (1)))     = ")
(num-zeroes '(0 0 0 (1 (1 (0 ( 0 ( 0))0) 0)) (1)))

(display "(num-zeroes '(0 0 0 (1 (1 (0 ( 0 ( 0))0) 0)) (1 0 0))) = ")
(num-zeroes '(0 0 0 (1 (1 (0 ( 0 ( 0))0) 0)) (1 0 0)))

;Problem 6
(display "\nProblem 6\n")
;Function that reverses a list

(define (nested-reverse ls)
  (define (nested-reverse-2 ls acc) ;acc is the accumulator
    (if (null? ls)
        acc
        (if (list? (car ls))
            (nested-reverse-2 (cdr ls) (cons (nested-reverse (car ls)) acc));  If adding  a list, reverse it first
            (nested-reverse-2 (cdr ls) (cons (car ls) acc)))))
  (nested-reverse-2 ls '()))
;Should be tail recursive

(display "(nested-reverse '(a b c))                         = ")
(nested-reverse '(a b c))

(display "(nested-reverse '(a b (c b (d f g) e) c))         = ")
(nested-reverse '(a b (c b (d f g) e) c)) 

(display "(nested-reverse '((a b c) 42 (do re mi (1 2 3)))) = ")
(nested-reverse '((a b c) 42 (do re mi (1 2 3)))) 