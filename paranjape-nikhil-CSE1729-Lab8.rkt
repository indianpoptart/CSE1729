"Lab 8"
"Nikhil Paranjape"
"2018-11-02"

;Problem 1
(display "\nProblem 1\n")
;a mergesort problem

;List merger
(define merge-lists
    (lambda (L1 L2)
      (if (null? L1)
          L2
          (if (null? L2)
              L1
              (if (< (car L1) (car L2))
                  (cons (car L1) (merge-lists (cdr L1) L2))
                  (cons (car L2) (merge-lists (cdr L2) L1)))))))

;Basic even number puller for lists
(define even-numbers
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              '()
              (cons (car (cdr L)) (even-numbers (cdr (cdr L))))))))

;Basic odd number puller for lists
(define odd-numbers
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              (list (car L))
              (cons (car L) (odd-numbers (cdr (cdr L))))))))

(display "\n ---------- HELPERS ----------
;List merger
(define merge-lists
    (lambda (L1 L2)
      (if (null? L1)
          L2
          (if (null? L2)
              L1
              (if (< (car L1) (car L2))
                  (cons (car L1) (merge-lists (cdr L1) L2))
                  (cons (car L2) (merge-lists (cdr L2) L1)))))))

;Basic even number puller for lists
(define even-numbers
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              '()
              (cons (car (cdr L)) (even-numbers (cdr (cdr L))))))))

;Basic odd number puller for lists
(define odd-numbers
    (lambda (L)
      (if (null? L)
          '()
          (if (null? (cdr L))
              (list (car L))
              (cons (car L) (odd-numbers (cdr (cdr L))))))))
---------- End Helper ----------\n")

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
(display "\nProblem 1b\n")

;Merge sorts a list L using merge-lists and even/odd-numbers
(define merge-sort
    (lambda (L)
      (if (null? L)
          L
          (if (null? (cdr L))
              L
              (merge-lists
                (merge-sort (odd-numbers L))
                (merge-sort (even-numbers L)))))))

(display "(merge-sort '(8 6 3 2 1 5 7 4)) = ")
(merge-sort '(8 6 3 2 1 5 7 4))

;Problem 2a

;Tree maker
(define (make-tree value left right)
  ;; produces a tree with value at the root , and
  ;; left and right as its subtrees. 
  (list value left right))

(define (value tree)
  (car tree))
;Left Tree
(define (left tree)
  (cadr tree))
;Right tree
(define (right tree)
  (caddr tree))    

(display "\nProblem 2a\n")
;Calculates the number of nodes in a tree 't'
(define tree-node-count
  (lambda (t)
    (cond ((null? t) 0)
        (else (+ 1
                 (tree-node-count (left  t))
                 (tree-node-count (right t)))))))

(define testtree (make-tree 1
                   (make-tree 3
                     (make-tree 7 '() '())
                     (make-tree 9 '() '()))
                   (make-tree 5 '() '())))

(define testtree2 (make-tree 1
                   (make-tree 3
                     (make-tree 7
                       (make-tree 4 '() '())
                       (make-tree 6 '() '()))
                     (make-tree 9 '() '()))
                   (make-tree 5
                     (make-tree 1 '() '())
                     (make-tree 3 '() '()))))

(define testtree3 (make-tree 1
                    (make-tree 2 '() '())
                    (make-tree 3 '() '())))

;testtree:
;                               1
;                              / \
;                             3   5
;                            / \
;                           7   9
(display "\ntesttree:
          1
         / |
       3   5
      / |
     7  9
\n")

;testtree2:
;                                 1  
;                              /     \
;                             3       5
;                            / \     / \
;                           7   9   1   3
;                          / \
;                         4   6
(display "\ntesttree2:
            1
         /    |
       3        5
      / |      / |
     7   9    1   3
    / |
   4   6
\n")

;testtree3
;        1
;       / \
;      2   3

(display "\ntesttree3:
         1
        / |
       2   3
\n")

(display "\n(tree-node-count testtree)  = ")
(tree-node-count testtree)

(display "\n(tree-node-count testtree2) = ")
(tree-node-count testtree2)

(display "\n(tree-node-count testtree3)  = ")
(tree-node-count testtree3)

;Problem 2b
(display "\nProblem 2b\n")
;A Lambda function that calculates tree height

(define tree-height
  (lambda (t)
    (cond ((null? t) 0)
          ((null? (or (left t) (right t))) 0)
          (else
           (+ 1 (max (tree-height (left t))
                     (tree-height (right t))))))))

(display "\n(tree-height testtree)  = ")
(tree-height testtree)

(display "\n(tree-height testtree2) = ")
(tree-height testtree2)

(display "\n(tree-height testtree3) = ")
(tree-height testtree3)

;Problem 2c
(display "\nProblem 2c\n")

;A lambda tree-map function that maps over a node with the function f
(define tree-map
  (lambda (f t)
    (if (null? t)
        t
        (make-tree (f (value t))
                   (tree-map f (left t))
                   (tree-map f (right t))))))

(display "\n(tree-map (lambda(x)(* 3 x)) testtree)  = ")
(tree-map (lambda(x)(* 3 x)) testtree)

(display "\n(tree-map (lambda(x)(* 3 x)) testtree2) = ")
(tree-map (lambda(x)(* 3 x)) testtree2)

(display "\n(tree-map (lambda(x)(* 3 x)) testtree3) = ")
(tree-map (lambda(x)(* 3 x)) testtree3)

(display "\n(tree-map (lambda(x)(+ 5 x)) testtree3) = ")
(tree-map (lambda(x)(+ 5 x)) testtree3)