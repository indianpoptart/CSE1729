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

(define (frequency-pair strings) (cons (car strings) (length strings)))

(define freq-list
  (lambda (L)
    (let* ((e (car L))
          (n (num-occurs e L))
          (L (remove-all e L)))
      (cons e n))))

(freq-list '(thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh thats the way uh-huh uh-huh i like it uh-huh uh-huh))

;Problem 3
(display "\nProblem 3\n")

(define (create-heap vw-pair left right)
    (list vw-pair left right))

(define (h-min heap)
    (car heap))

(define (left heap)
    (cadr heap))

(define (right heap)
    (caddr heap))

(define (insert vw-pair heap)
    (if (null? heap)
        (create-heap vw-pair '() '())
        (if (< (weight vw-pair) (weight (h-min heap)))
            (create-heap vw-pair (right heap) (insert (h-min heap) (left heap)))
            (create-heap (h-min heap) (right heap) (insert vw-pair (left heap))))))

(define (insert-list-of-pairs vw-pair-list heap)
    (if (null? vw-pair-list)
        heap
        (insert (car vw-pair-list)
                (insert-list-of-pairs (cdr vw-pair-list) heap))))

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
 ""
 )

;Problem 5
(display "\nProblem 5\n")

(define (push x S) (cons x S))
(define (top S) (car S))
(define (pop S) (cdr S))
(define (empty? S) (if (null? S) #t #f))

(define get-in-order
  (lambda (heap)
    (if
     