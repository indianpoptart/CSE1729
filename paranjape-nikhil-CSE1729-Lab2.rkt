"Lab 2"
"Nikhil Paranjape"
"2018-09-13"

(newline)

(display "problem 1a\n")
;Defining Pi
(define pi
  (* 2 (acos 0)))
"pi"
pi

(newline)

(display "problem 1b\n")
;Define an area of the circle function
(define (area-of-circle r)
  (* pi (expt r 2)))
"(area-of-circle 2)"
(area-of-circle 2)

(newline)

(display "problem 1c\n")
;Define a Surface Area of a Sphere function using area-of-circle function
(define (surface-area-of-sphere r)
  (* 4 (area-of-circle r)))
"(surface-area-of-sphere 3)"
(surface-area-of-sphere 3)

(newline)

(display "problem 1d\n")
;Define a Volume of Sphere function using surface-area-of-sphere function
(define (volume-of-sphere r)
  (/ (* r (surface-area-of-sphere r)) 3))
"(volume-of-sphere 6)" ; = 904.778
(volume-of-sphere 6)

(newline)

(display "problem 2\n")
;Function to compute nth value in a sequence
(define (seq n)
  (cond
    ((= n 1) 1)
    ((= n 2) 2)
    ((= n 3) 3)
    (else
     (+ (- (seq (- n 3))
           (seq (- n 2)))
        (seq (- n 1))))))

"(seq 4)"
(seq 4)

(newline)

(display "problem 3\n")
;Recursive function that computes Zeno's Dichotomy Paradox
(define (zeno n)
  (cond
    ((= n 0) 0)
    (else
     (+ (zeno (- n 1)) (/ 1 (expt 2 n))))))
"(zeno 7)"
(zeno 7)

(newline)

(display "problem 4a\n")
;Function that determines whether a non-negative number is even or not
(define (even-nn-int? n)
  (cond
    ((= n 0) #f)
    ((= n 1) #t)
    (else
     (zero?(- n (* 2 (quotient n 2)))))))

"(even-nn-int? 33)"
(even-nn-int? 33)
"(even-nn-int? 34)"
(even-nn-int? 34)

(newline)

(display "problem 4b\n")
;Write a function even-int? that determines whether any integer is even.
(define (even-int? n)
  (cond
    ((even-nn-int? n) #t)
    (else #f)))

"(even-int? -3)"
(even-int? -3)
"(even-int? -2)"
(even-int? -2)
"(even-int? 2)"
(even-int? 2)
"(even-int? 3)"
(even-int? 3)

(newline)
(display "problem 4c\n")
;Finally, we define odd-int? that determines whether any integer is odd.
(define (odd-int? n)
  (cond
    ((even-nn-int? n) #f)
    (else #t)))

"(odd-int? -3)"
(odd-int? -3)
"(odd-int? -2)"
(odd-int? -2)
"(odd-int? 2)"
(odd-int? 2)
"(odd-int? 3)"
(odd-int? 3)

;Nikhil Paranjape