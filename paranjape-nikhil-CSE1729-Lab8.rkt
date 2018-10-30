"Lab 8"
"Nikhil Paranjape"
"2018-11-02"

;Problem 1
(display "\nProblem 1\n")
; a mergesort function

;List merger
(define merge-lists
    (lambda (l1 l2)
      (if (null? l1)
          l2
          (if (null? l2)
              l1
              (if (< (car l1) (car l2))
                  (cons (car l1) (merge-lists (cdr l1) l2))
                  (cons (car l2) (merge-lists (cdr l2) l1)))))))

;Basic even number puller for lists
(define even-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              '()
              (cons (car (cdr l)) (even-numbers (cdr (cdr l))))))))

;Basic odd number puller for lists
(define odd-numbers
    (lambda (l)
      (if (null? l)
          '()
          (if (null? (cdr l))
              (list (car l))
              (cons (car l) (odd-numbers (cdr (cdr l))))))))
;Problem 1a
(display "\nProblem 1a.\n")
;basic list splitter
(define split
  (lambda (L)
	(cons (odd-numbers L) (cons (even-numbers L) `()))))

(display "(split '(a b c d e f g h i j k l m n o p)) = ")
(split '(a b c d e f g h i j k l m n o p))

(display "(split '(a m n o p)) = ")
(split '(a m n o p))

;Problem 1b
(display "\nProblem 1b.\n")

(define merge-sort
    (lambda (l)
      (if (null? l)
          l
          (if (null? (cdr l))
              l
              (merge-lists
                (merge-sort (odd-numbers l))
                (merge-sort (even-numbers l)))))))

(display "(merge-sort '(8 6 3 2 1 5 7 4)) = ")
(merge-sort '(8 6 3 2 1 5 7 4))