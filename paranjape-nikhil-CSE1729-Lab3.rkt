"Lab 3"
"Nikhil Paranjape"
"2018-09-20"

"License: https://github.com/indianpoptart/CSE1729/blob/master/LICENSE"

(newline)

(display "Problem 1a\n")
;A function that computes the number of Jeanie's ancestors in the nth previous generation
;Without using the expt function

(define (ancestor-compute n)
  (if (= n 1)
      2
      (* 2 (ancestor-compute (- n 1)))))

"(ancestor-compute 3)"
(ancestor-compute 3)

(newline)

;TODO Problem 1b
(display "Problem 1b\n")

(define (num-ancestors n)
  (if (= n 1)
      2
      (+ (ancestor-compute n) (num-ancestors (- n 1)))))

"(num-ancestors 4)"
(num-ancestors 4)

(newline)

(display "problem 2a\n")
;Pell's Numbers

(define (pell-num n)
  (cond
    ((= 0 n) 0)
    ((= 1 n) 1)
    (else
     (+ (* 2 (pell-num (- n 1))) (pell-num (- n 2))))))

"(pell-num 7)"
(pell-num 7)

(newline)

(display "Problem 2b\n")
;comp-pell-num, returns the nth companion pell number

(define (comp-pell-num n)
  (cond
    ((= 0 n) 2)
    ((= 1 n) 2)
    (else
     (+ (* 2 (comp-pell-num (- n 1))) (comp-pell-num (- n 2))))))

"(comp-pell-num 7)"
(comp-pell-num 7)

(newline)

(display "Problem 2c\n")
;TODO Problem 2c

(define (pell-comp n)
  (/ (/ (comp-pell-num n) 2) (pell-num n)))

"(pell-comp 6)"
(pell-comp 6)


(newline)

(display "Problem 3a\n")
;Design a Scheme function fastexp which calculates be for any e ≥ 0 by the rule:
;  ee2
;1 ife=0,
;b = (b2) ifeiseven,
;  b∗be−1 if e is odd.

(define (square x)
  (* x x))

(define (fastexp b e)
  (cond
    ((= e 0) 1)
    ((even? e) (square (fastexp b (/ e 2))))
    ((odd? e) (* b (fastexp b (- e 1))))))

"(fastexp 3 3)"
(fastexp 3 3)