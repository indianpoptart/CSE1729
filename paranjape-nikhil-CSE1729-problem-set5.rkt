"Problem Set 5"
"Nikhil Paranjape"
"2018-11-05"

;Problem 1a
(display "\nProblem 1a\n")

(define is-prefix?
  (lambda (L1 L2)
    (cond
      ((null? L1) #t)
      ((null? L2) #f)
      ((= (car L1)(car L2))
       (is-prefix? (cdr L1)(cdr L2)))
      (else
       #f))))

(display "\n(is-prefix? '(1) '(1 2 3)) = ")
(is-prefix? '(1) '(1 2 3))

(display "\n(is-prefix? '(2) '(1 2 3)) = ")
(is-prefix? '(2) '(1 2 3))

(display "\n(is-prefix? '(7) '(7 6 5 4 3 2 1)) = ")
(is-prefix? '(7) '(7 6 5 4 3 2 1))

;Problem 1b
(display "\nProblem 1b\n")

(define longest-common-prefix
  (lambda (L1 L2)
    (cond
      ((null? L1) '())
      ((null? L2) '())
      ((= (car L1)(car L2))
       (cons (car L1)(longest-common-prefix (cdr L1)(cdr L2))))
      (else '()))))

(display "\n(longest-common-prefix '(1 2) '(1 2 3)) = ")
(longest-common-prefix '(1 2) '(1 2 3))

(display "\n(longest-common-prefix '(1 2 3) '(1 2 3 4 5 6)) = ")
(longest-common-prefix '(1 2 3) '(1 2 3 4 5 6))

(display "\n(longest-common-prefix '(1 2) '(3 6 7 1 2 9 4)) = ")
(longest-common-prefix '(1 2) '(3 6 7 1 2 9 4))

;Problem 2a
(display "\nProblem 2a\n")

(define (list-gen from to)
   (if (> from to)
      '()
      (cons from (list-gen (+ from 1) to))))

(define list-gen
  (lambda (a b)
      (if (> a b)
         '()
         (cons a (list-gen (+ a 1) b)))))

(define (gen-consecutive f a b)
  (map f (list-gen a b)))

(display "\n(gen-consecutive (lambda(x)(* x x)) 1 10)   = ")
(gen-consecutive (lambda(x)(* x x)) 1 10)

(display "\n(gen-consecutive (lambda(x)(* x x x)) 1 10) = ")
(gen-consecutive (lambda(x)(* x x x)) 1 10)

(display "\n(gen-consecutive (lambda(x)(* x x)) 10 20)  = ")
(gen-consecutive (lambda(x)(* x x)) 10 20)



;Problem 2b
(display "\nProblem 2b\n")

(define gen-sequence
  (lambda (f a b next)
    (if (> a b)
        '()
        (cons (f a a) (gen-sequence f (next a) b next)))))

(define factorial
  (lambda (n acc)
     (if( = n 0 ) 
        acc
      (factorial (- n 1)  (* acc n )))))

(display "\n(gen-sequence factorial 1 5 (lambda(x)(+ x 2)))  = ")
(gen-sequence factorial 1 5 (lambda(x)(+ x 2)))

(display "\n(gen-sequence factorial 1 10 (lambda(x)(+ x 4))) = ")
(gen-sequence factorial 1 10 (lambda(x)(+ x 4)))

(display "\n(gen-sequence factorial 1 15 (lambda(x)(+ x 6))) = ") 
(gen-sequence factorial 1 15 (lambda(x)(+ x 6)))

;Problem 3
(display "\nProblem 3\n")

(define filter
  (lambda (f L1)
    (cond
      ((null? L1) '())
      ((f (car L1))
       (cons (car L1)(filter f (cdr L1))))
      (else
       (filter f (cdr L1))))))

(display "\n(filter even? '(1 2 3 4 5 6 7)) = ")
(filter even? '(1 2 3 4 5 6 7))

(display "\n(filter odd? '(1 2 3 4 5 6 7))  = ") 
(filter odd? '(1 2 3 4 5 6 7))

(display "\n(filter odd? '(1 2 3 4 5 6 7 8 9 10 11 12)) = ")
(filter odd? '(1 2 3 4 5 6 7 8 9 10 11 12))

;Problem 4a
(display "\nProblem 4a\n")

(define value-at-position
  (lambda (L1 k)
    (if (= k 1)
        (car L1)
        (value-at-position (cdr L1) (- k 1)))))

(display "\n(value-at-position '(1 2 3 4 5 6) 3)   = ")
(value-at-position '(1 2 3 4 5 6) 3)

(display "\n(value-at-position '(7 6 5 4 3 2 1) 3) = ")
(value-at-position '(7 6 5 4 3 2 1) 3)

(display "\n(value-at-position '(7 6 8 3 2 9 22 12 35 12) 7) = ")
(value-at-position '(7 6 8 3 2 9 22 12 35 12) 7)

;Problem 4b
(display "\nProblem 4b\n")

(define (is-prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define (range a b)
  (if (> a b)
      '()
      (cons a (range (+ 1 a) b))))

(define consecutive-ints
  (lambda (n m)
    (cond
      ((= n m) (list n))
        (else (cons n (range ((if (< n m) + -) n 1) m))))))

(define nth-prime-between
  (lambda (a b n)
    (value-at-position (filter is-prime? (consecutive-ints a b)) n)))

(display "\n(nth-prime-between 1 10 2)    = ")
(nth-prime-between 1 10 2)

(display "\n(nth-prime-between 1 100 6)   = ")
(nth-prime-between 1 100 6)

(display "\n(nth-prime-between 1 1000 16) = ")
(nth-prime-between 1 1000 16)

;Problem 5a
(display "\nProblem 5a\n")

(define square
  (lambda (x) (* x x)))

(define (make-complex a b)
  (cons a b))

(define (real x)
  (car x))

(define (imag x)
  (cdr x))

(define sgn
      (lambda (v)
        (cond ((< v 0) -1)
              ((= v 0) 0)
              (else 1))))

(define complex-sqrt
  (lambda (x)
    (make-complex
     (sqrt (/ (+ (real x) (sqrt (+ (square (real x)) (square (imag x))))) 2))
     (* (sgn (imag x))(sqrt (/ (+ (- (real x)) (sqrt (+ (square (real x)) (square (imag x))))) 2))))))

(display "
(define blop (make-complex 5 6))

(define blip (make-complex 2 4))

(define bleep (make-complex 5 0))
\n")

(define blop (make-complex 5 6))

(define blip (make-complex 2 4))

(define bleep (make-complex 5 0))

(display "\n(complex-sqrt blop)  = ")
(complex-sqrt blop)

(display "\n(complex-sqrt blip)  = ")
(complex-sqrt blip)

(display "\n(complex-sqrt bleep) = ")
(complex-sqrt bleep)

;Problem 5b
(display "\nProblem 5b\n")

(display "\n(complex-sqrt (make-complex -2 0)) = ")
(complex-sqrt (make-complex -2 0))

(display "\nWhen B = 0 there is a fault in complex-sqrt, the imaginary portion returns 0")

;Problem 5c
(display "\nProblem 5c\n")

(define better-complex-sqrt
  (lambda (x)
    (define sgn
      (lambda (v)
        (cond ((< v 0) -1)
              (else 1))))
    (let ((root (sqrt (+ (square (real x)) (square (imag x))))))
      (make-complex (sqrt (/ (+ (real x) root) 2))
                    (* (sgn (imag x))
                       (sqrt (/ (- root (real x)) 2)))))))

(display "\n(better-complex-sqrt blop)  = ")
(complex-sqrt blop)

(display "\n(better-complex-sqrt blip)  = ")
(complex-sqrt blip)

(display "\n(better-complex-sqrt bleep) = ")
(better-complex-sqrt bleep)