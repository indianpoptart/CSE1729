"Lab 6"
"Nikhil Paranjape"
"2018-10-18"

;Problem 1a
(display "\nProblem 1a\n")
;Constructs a complex number with real part a and imaginary part b

(define (make-complex a b)
  (cons a b))

"(make-complex 3 2)"
(make-complex 3 2)

"(make-complex 4 3)"
(make-complex 4 3)

;Problem 1b
(display "\nProblem 1b\n")
;Returns the real part of a complex number x
(define (real x)
  (car x))

(display "\n(define blop (make-complex 3 2))\n")
(define blop (make-complex 3 2))

"(real blop)"
(real blop)

"(real (make-complex 4 5))"
(real (make-complex 4 5))

;Problem 1c
(display "\nProblem 1c\n")
; Returns the imaginary part of the complex number x

(define (imag x)
  (cdr x))

"(imag blop)"
(imag blop)

;Problem 2a
(display "\nProblem 2a\n")
;Returns the sum of two complex numbers

(define bdo (make-complex 5 6))

(define (complex-add x y)
  (make-complex (+ (real x) (real y))
                (+ (imag x) (imag y))))
(display "blop = ") blop
(display "bdo = ") bdo

"(complex-add blop bdo)"
(complex-add blop bdo)

"(complex-add blop blop)"
(complex-add blop blop)

;Problem 2b
(display "\nProblem 2b\n")
;Returns the difference of two complex numbers
(define (complex-sub x y)
  (make-complex (- (real x) (real y))
                (- (imag x) (imag y))))

(display "blop = ") blop
(display "bdo = ") bdo

"(complex-sub blop bdo)"
(complex-sub blop bdo)

"(complex-sub blop blop)"
(complex-sub blop blop)

;Problem 2c
(display "\nProblem 2c\n")
;Returns the product of two complex numbers

(define (complex-mult x y)
  (make-complex (- (* (real x) (real y)) (* (imag x) (imag y)))
                (+ (* (imag x) (real y)) (* (real x) (imag y)))))

(display "blop = ") blop
(display "bdo = ") bdo

(display "(complex-mult blop bdo) = ")
(complex-mult blop bdo)

(display "(complex-mult blop blop) = ")
(complex-mult blop blop)

;Problem 4a
(display "\nProblem 4a\n")
;Counts the number of positive numbers in a list

(define (count-positives lst)
  (cond
    ((null? lst) 0)
    ((> (car lst) 0)
     (+ 1 (count-positives (cdr lst))))
    (else
     (count-positives (cdr lst)))))

(display "(count-positives (list 1 -23 0 -11 3 1002)) = ")
(count-positives (list 1 -23 0 -11 3 1002))

;Problem 4b
(display "\nProblem 4b\n")
;Adds up the elements in a list of numbers

(define (sum-list lst)
  (cond
    ((null? lst) 0)
    (else
     (+ (car lst) (sum-list (cdr lst))))))

(display "(sum-list (list 1 2 3 4 5)) = ")
(sum-list (list 1 2 3 4 5))

(display "(sum-list (list 1 -23 0 -11 3 1002)) = ")
(sum-list (list 1 -23 0 -11 3 1002))

;Problem 4c
(display "\nProblem 4c\n")
;Consecutive-Ints Evaluates to a list of numbers from a to b, where a and b are integers; if a > b the result is the empty list '()
;Consecutive-Squares evaluates to the list of perfect squares from a^2 to b^2

(define (consecutive-ints a b)
  (cond
    ((> a b) '())
    (else
     (cons a (consecutive-ints (+ a 1) b)))))

(define (consecutive-squares a b)
  (cond
    ((> a b) '())
    