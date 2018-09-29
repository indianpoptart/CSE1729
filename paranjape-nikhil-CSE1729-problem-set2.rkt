"Problem Set 2"
"Nikhil Paranjape"
"2018-09-30"

"License: https://github.com/indianpoptart/CSE1729/blob/master/LICENSE"

(newline)

(define (number-sum n)
  (if (= n 0)
      0
      (+ n (number-sum (- n 1)))))

(display "Problem 1a\n")
;Adapt number-sum to even-sum. It computes the sum of the first n even numbers
;even-sum 4 should return 12
(define (even-sum n)
  (if (= n 0)
      0
      (+ (- (* 2 n) 2) (even-sum (- n 1)))))

"(even-sum 4)"
(even-sum 4)

"(even-sum 6)"
(even-sum 6)

(newline)

(display "Problem 1b\n")
;What does the sequence look like?
(display "The sequence of numbers seem to follow this expression: n(n-1)")

(newline)

(display "Problem 1c\n")
;Adapt the function into sum-from-to, which computes the sum of all integers from a to be
;(sum-from-to 3 5) should evaluate to 12

(define (sum-from-to a b)
  (if (> a b)
      0
      (+ a (sum-from-to (+ a 1) b))))

"(sum-from-to 3 5)"
(sum-from-to 3 5)

"(sum-from-to 6 10)"
(sum-from-to 6 10)

(newline)

(display "Problem 2\n")
;Function diff-prod that, given a positive integer k > 1,
;computes (1-1/2)(1-1/3)...(1-1/k)

(define (diff-prod-recurse k)
  (if (= k 2)
      (/ 1 2)
      (* (- 1 (/ 1 k)) (diff-prod-recurse (- k 1)))))

"(diff-prod-recurse 4)"
(diff-prod-recurse 4)

"(diff-prod-recurse 7)"
(diff-prod-recurse 7)

(define (diff-prod k)
  (/ 1 k))

"(diff-prod 25)"
(diff-prod 25)

(newline)

(display "Problem 3a\n")
;Sum of z^k, from 0 to k

(define (finite-sim-of-powers z k)
  (if (= k 0)
      1
      (+ (expt z k)
         (finite-sim-of-powers z (- k 1)))))

(display "\nProblem 3b\n")
;Evaluates the number of terms in the infinite sum neeed to be within tol, that is,
;The smallest k such that the difference between 1/(1-z) and (finite-sum-of-powers zk) is
;Within tol

;Helper
(define (first-value-k-or-higher z tol k)
  (cond
    ((< (- (/ 1 (- 1 z)) (finite-sim-of-powers z k)) tol) k)
    (else
     (first-value-k-or-higher z tol (+ k 1)))))
"---BEGIN HELPER----"
(display "
(define (first-value-k-or-higher z tol k)
  (cond
    ((< (- (/ 1 (- 1 z)) (finite-sim-of-powers z k)) tol) k)
    (else
     (first-value-k-or-higher z tol (+ k 1)))))
")
"--- END HELPER ----"

(define (terms-needed z tol)
  (first-value-k-or-higher z tol 1))



"(terms-needed 5 2)"
(terms-needed 5 2)