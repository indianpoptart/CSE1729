"Lab 9"
"Nikhil Paranjape"
"2018-11-28"

;Problem 1
(display "\nProblem 1\n")
;Evaluates to the number of times the symbol appears in the list

(define num-occurs
  (lambda (sym list)
    (cond
      ((null? list) 0)
      ((eq? sym (car list))
       (+ 1 (num-occurs sym (cdr list))))
      (else
       (num-occurs sym (cdr list))))))

(display "(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh)) = ")
(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh))

(display "(num-occurs 'a '(a b c (not (c b a)))) = ")
(num-occurs 'a '(a b c (not (c b a))))

;Problem 2

(display "\nProblem 2\n")
;Takes a list of symbols as a parameter and returns a list of symbol-frequency pairs.

(define (remove-all x L)
  (cond ((null? L) L)
        ((list? (car L))
         (cons (remove-all x (car L)) (remove-all x (cdr L))))
        ((equal? x (car L)) (remove-all x (cdr L)))
        (else (cons (car L) (remove-all x (cdr L))))))

(define freq-list
  (lambda (L)
    (let LF ((L L) (rslt '()))
      (cond ((null? L) rslt)
            ((assoc (car L) rslt) =>
                                     (lambda (pair) 
                                       (set-cdr! pair (+ 1 (cdr pair)))
                                       (LF (cdr L) rslt)))
            (else (LF (cdr L) 
                           (cons (cons (car L) 1) rslt)))))))

(display "(freq-list '(thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh)) = \n")
(freq-list '(thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh))

(display "(freq-list '(row row your boat boat boat row your boat boat boat)) = ")
(freq-list '(row row your boat boat boat row your boat boat boat)) 

;Problem 3a
(display "\nProblem 3a\n")

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (weight-leaf x) (caddr x))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(display "Helper:")
(display "
(define (leaf? object)
  (eq? (car object) 'leaf))

(define (weight-leaf x) (caddr x))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
")

(define (create-heap vw-pair left right)
    (list vw-pair left right))

;Problem 3b
(display "\nProblem 3b\n")

(define (h-min heap)
    (car heap))

(h-min '(a b c))

;Problem 3c
(display "\nProblem 3c\n")

(define (left heap)
    (cadr heap))

;Problem 3d
(display "\nProblem 3d\n")

(define (right heap)
    (caddr heap))

;Problem 3e
(display "\nProblem 3e\n")

(define (insert vw-pair heap)
    (if (null? heap)
        (create-heap vw-pair '() '())
        (if (< (weight vw-pair) (weight (h-min heap)))
            (create-heap vw-pair (right heap) (insert (h-min heap) (left heap)))
            (create-heap (h-min heap) (right heap) (insert vw-pair (left heap))))))

;Problem 3f
(display "\nProblem 3f\n")
(define (insert-list-of-pairs vw-pair-list heap)
    (if (null? vw-pair-list)
        heap
        (insert (car vw-pair-list)
                (insert-list-of-pairs (cdr vw-pair-list) heap))))

;Problem 3g
(display "\nProblem 3g\n")

(define (combine-heaps H1 H2)
    (cond ((null? H1) H2)
          ((null? H2) H1)
          ((< (h-min H1) (h-min H2))
           (create-heap (h-min H1)
                        H2
                        (combine-heaps (left H1) (right H1))))
          (else
           (create-heap (h-min H2)
                        H1
                        (combine-heaps (left H2) (right H2))))))

(define (remove-minimum H)
    (combine-heaps (left H) (right H)))

;Problem 4
(display "\nProblem 4\n")

;When you are coding and debugging, how do you generate test cases?
;And how can you tell if your answers from these tests are correct?
;Your answer should be a single paragraph. When considering this question,
;think about what you have done on recent assignments, not just on today's lab.


(display
 "For generating test cases, I check what the function is supposed to do, or rather, what I would like the function to do.
If I have a function (square) that takes a variable and multiplies it by itself. Test cases can also be checked with slightly
modified versions of given examples. Suppose a function can determine the amount of nested lists inside a list.
You can simply put a set number of nested lists and check whether the inputted lists matches in the function."
 )

;Problem 5
(display "\nProblem 5\n")

(define (push x S) (cons x S))
(define (top S) (car S))
(define (pop S) (cdr S))
(define (empty? S) (if (null? S) #t #f))


(define get-in-order
  (lambda (heap)
    (cond
      ((empty? heap) heap)
      (else
       (cons (pop heap) (get-in-order (cdr heap)))))))

(define (heapsort pair-list) (get-in-order (insert-list-of-pairs pair-list '())))

;Problem 6
(display "\nProblem 6\n")
