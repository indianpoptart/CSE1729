"Problem Set 1"
"Nikhil Paranjape"
"2018-09-13"
(display "License: https://github.com/indianpoptart/CSE1729/blob/master/LICENSE")
(newline)

(display "problem 1a\n")
;Rewrite (22 + 42) * (54 * 99).

 "(* (+ 22 42) (* 54 99))\n"
(* (+ 22 42) (* 54 99))

(newline)
(display "problem 1b\n")
"(* 99 (* 54 (+ 22 42)))\n"
(* 99 (* 54 (+ 22 42)))

(newline)
(display "problem 1c\n")
;Rewrite 64 * 102 + 16 * (44/22)
"(+ (* 64 102) (* 16 (/ 44 22)))\n"
(+ (* 64 102) (* 16 (/ 44 22)))

(newline)
(display "problem 2a\n")
;Reflect
(display "The expressions (a) & (b) have a different order of operations\n")
(display "In scheme the formatting of each number, when it is processed and, which procedure is being run")

(newline)
(display "problem 2b\n")
;Reflect
(display "Rules of precedence are necessary for arithmetic operations in scheme ")
;TODO

(newline)
(display "problem 3a\n")
;Cube function: cube(x) = x^3
(define (cube x)
  (expt x 3))
"(cube 3)"
(cube 3)

(newline)
(display "problem 3b\n")
;Q function before cube
(define (q x)
  (+ 21 (expt x 5) (* 11 (expt x 4)) (* 24 (expt x 3)) (- x)))

(define (p x)
  (cube (q x)))

"(p 4)"
(p 4)

(newline)
(display "problem 3c\n")
;tenth(x) = x^10
(define (tenth x)
  (* x (cube x) (cube x) (cube x)))
"(tenth 2)"
(tenth 2)

(newline)
(display "problem 3d\n")
(define (hundreth x)
  (* (tenth (tenth x))))
"(hundreth 2)"
(hundreth 2)

(newline)
(display "problem 3e\n")
;How to test if hundreth gave correct answer
"You can run the built in procedure expt; eg (expt 2 100)"

(newline)
(display "problem 3f\n")
;Reflect, difficulty of defining with *
"The difficulty with * would require 100 x characters to do hundreth(x) = x^100"

(newline)
(display "problem a\n")
;(y-value x b m) is the function for slope: mx + b.
;
;X value
;y-intercept b
;slope m

(define (y-value m x b)
  (+ (* m x) b))
"(y-value 2 3 4)"
(y-value 2 3 4)
