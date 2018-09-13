"Problem Set 1 / Project 1"
"Nikhil Paranjape"
"2018-09-13"

"License: https://github.com/indianpoptart/CSE1729/blob/master/LICENSE"

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
(display "Yes, rules of precedence are necessary for arithmetic operations in scheme ")

(newline)

(display "problem 3a\n")
;Cube function: cube(x) = x^3
(define (cube x)
  (* x x x))
"(cube 3)"
(cube 3)

(newline)

(display "problem 3b\n")
;Q function before cube
(define (q x)
  (+ (* x x x x x) (* 11 (* x x x x)) (* 24 (* x x x)) (- x) 21))

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
"You can run the built in procedure expt; eg (expt 2 100) or,"
"tediously write in parenthesis 100 x's after a *."

(newline)

(display "problem 3f\n")
;Reflect, difficulty of defining with *
"The difficulty with * would require 100 x characters to do hundreth(x) = x^100"

(newline)

(display "problem 4a\n")
;(y-value x b m) is the function for slope: mx + b.
;
;X value
;y-intercept b
;slope m

(define (y-value m x b)
  (+ (* m x) b))
"(y-value 2 3 4)"
(display "Y-Value: ")
(y-value 2 3 4)

(newline)

(display "problem 4b\n")
;A function of four parameters (the x and y values of two points)
;that calculates the slope of a line through those points
;(x1,y1) and (x2,y2).

(define (points-slope x1 y1 x2 y2)
  (/ (- y2 y1) (- x2 x1)))

"(points-slope 3 2 7 -4)"
(points-slope 3 2 7 -4)

(newline)
(display "problem 4c\n")
;A function of four parameters (the x and y values of two points)
;that calculates the y-intercept of a line through points
;(x1,y1) and (x2,y2).

(define (points-intercept x1 y1 x2 y2)
  (- y2 (* x2 (points-slope x1 y1 x2 y2))))

"(points-intercept 3 2 7 -4)"
(points-intercept 3 2 7 -4)

(newline)

(display "problem 4d\n")
;a function of eight parameters (the x and y values of four points),
;returns true if the line through points (x1,y1) and (x2,y2) is parallel to the line
;through points (x3,y3) and (x4,y4). You may assume that points (x1, y1) and (x2, y2) are distinct,
;as are (x3, y3) and (x4, y4).

(define (on-parallels? x1 y1 x2 y2 x3 y3 x4 y4)
  (cond
    ((= (points-slope x1 y1 x2 y2) (points-slope x3 y3 x4 y4)) #t)
    (else #f)))

"(on-parallels? 2 4 6 5 2 3 6 4)"
(on-parallels? 2 4 6 5 2 3 6 4)

(newline)

;Quadratic stuff:
(define (top a b c)
  (sqrt(- (expt b 2) (* 4 a c))))
(define (bottom a)
  (* 2 a))

(display "problem 5a\n")
;(root1 a b c) that gives us the root corresponding to the plus in the ± in the quadratic
;formula (that is, calculate (−b+ √b2−4ac)/2a).

(define (root1 a b c)
  (/ (+ (- b) (top a b c)) (bottom a)))

"(root1 1 2 1)"
(root1 1 2 1)

(newline)

(display "problem 5b\n")
;(root2 a b c) that gives us the root corresponding to the minus in the ± in the quadratic √
;formula (that is, calculate (−b - √b2−4ac)/2a).
(define (root2 a b c)
  (/ (- (- b) (top a b c)) (bottom a)))

"(root2 1 2 1)"
(root2 1 2 1)

(newline)

(display "problem 5c\n")
; Calculate the number of distinct roots to the equation ax^2 + bx + c = 0, a ≠ 0 (which will either be 1 or 2).

(define (number-of-roots a b c)
  (cond
    ((= 0 a) (display "Variable 'a' cannot be 0."))
    ((zero? (top a b c)) 1)
    ((positive? (top a b c)) 2)
    (else
     (display "Non-real Answer(s)"))))

"(number-of-roots 1 2 1)"
(number-of-roots 1 2 1)

(newline)

(display "problem 5d\n")
;A function that evaluates to #t when the roots of ax^2 + bx + c = 0,a 6= 0 are real numbers

(define (real-roots? a b c)
  (cond
    ((= 0 a) (display "Variable 'a' cannot be 0."))
    ((real? (top a b c)) #t)
    (else
     #f)))

"(real-roots? 1 2 1)"
(real-roots? 1 2 1)

"(real-roots? 1 -5 -21)"
(real-roots? 1 -5 -21)
"(real-roots? 1 5 21)"
(real-roots? 1 5 21)
    
(newline)

(display "problem 6\n")
;Given a 2-by-2 matrix M =a b c d, write
;a simple function that determines when the 2×2matrix M is lone-some.

;Matrix Determinant
(define (det2x2 a b
                c d)
  (- (* a d) (* b c)))

(define (lonesome a b c d)
  (cond
    ((= 1 (det2x2 a b c d)) #t)
    ((= -1 (det2x2 a b c d)) #t)
    (else
     #f)))

"(lonesome 1 0 0 1)"
(lonesome 1 0 0 1)

"(lonesome 0 1 1 0)"
(lonesome 0 1 1 0)

"(lonesome 1 0 5 0)"
(lonesome 1 0 5 0)
"(lonesome 0 3 5 0)"
(lonesome 0 3 5 0)

