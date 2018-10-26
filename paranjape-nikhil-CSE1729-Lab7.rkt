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

(define (dot-prod-with-map L1 L2)
  (apply + (map * L1 L2)))

(display "(dot-prod-with-map '(1 2 3) '(4 5 6)) = ")
(dot-prod-with-map '(1 2 3) '(4 5 6))
