#lang racket
"Problem Set 3"
"Nikhil Paranjape"
"2018-10-22"

;Problem 1
(display "\nProblem 1\n")
;Returns (-1)^k (x^2k / (2k)!

(define (new-cos x n)
  (define (factorial j)
    (if (= j 0)
        1
        (* j (factorial (- j 1)))))
  (if (= n 0)
      x
      (let ((z (* 2.0 n)))
        (+ (* (expt -1 n)
            (/ (expt x z)
               (factorial z)))
         (new-cos x (- n 1))))))

(display "(newcos 1 3)  = ")
(new-cos 1 3)

(display "(newcos 1 7)  = ")
(new-cos 1 7)

(display "(newcos 1 11) = ")
(new-cos 1 11)

;Problem 2
(display "\nProblem 2\n")

(define (harmonic n)
  (cond ((<= n 1) 1)
        (else (+ (/ 1 n) (harmonic (- n 1))))))

(display "\n----- HELPER ----- ")
(display "
(define (harmonic n)
  (cond ((<= n 1) 1)
        (else (+ (/ 1 n) (harmonic (- n 1))))))
")
(display "----- HELPER ----- \n")

(newline)

(define (euler-approx n)
  (abs (- (harmonic n) (log n))))

(display "(euler-approx 3)  = ")
(euler-approx 3)

(display "(euler-approx 7)  = ")
(euler-approx 7)

(display "(euler-approx 11) = ")
(euler-approx 11)

;Problem 3a
(display "\nProblem 3a\n")

(define (fast-fib n)
  (define (fib-pair n)
    (if (= n 1)
        (cons 1.0 0)
        (let ((prev-pair (fib-pair (- n 1))))
          (cons (+ (car prev-pair)
                   (cdr prev-pair))
                (car prev-pair)))))
  (car (fib-pair n)))

(display "fib-iter has 1 less iteration than fib\n")

;Problem 3b
;fn = (Fib n) / (Fib n-1)
;computes fn (given n as a parameter)

(display "\nProblem 3b\n")

(define (fib-ratio n)
  (/ (fast-fib n) (fast-fib (- n 1))))

(display "(fib-ratio 20) = ")
(fib-ratio 20)

(display "(fib-ratio 21) = ")
(fib-ratio 21)

(display "(fib-ratio 22) = ")
(fib-ratio 22)


;Problem 4a
(display "\nProblem 4a\n")

(define (golden n)
  (cond
    ((= n 1) 2)
    (else
     (+ 1.0 (/ 1.0 (golden (- n 1)))))))

(display "(golden 6)  = ")
(golden 6)

(display "(golden 10) = ")
(golden 10)

(display "(golden 14) = ")
(golden 14)

;Problem 4b
(display "\nProblem 4b\n")

(define (golden-approx n d k)
  (define (iter i res)
    (if (zero? i)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter (- k 1) (/ (n k) (d k))))

(display "(golden-approx 1.0 1 6)  = ")
(+ 1.0 (golden-approx (lambda (x) 1) (lambda (x) 1) 6))

(display "(golden-approx 1.0 1 10) = ")
(+ 1.0 (golden-approx (lambda (x) 1) (lambda (x) 1) 10))

(display "(golden-approx 1.0 1 14) = ")
(+ 1.0 (golden-approx (lambda (x) 1) (lambda (x) 1) 14))

;Problem 5
;Pi-approx gives you
;4 times the proportion of n random darts that end up inside the circle.

(display "\nProblem 5\n")

(define (square x)
  (* x x))

;P func
(define (monte-carlo-sampling n)
  (let ((x (- (* 2 (random)) 1))
        (y (- (* 2 (random)) 1)))
    (cond ((= n 0)
           0)
          (else
           (cond ((< (sqrt (+ (square x) (square y))) 1)
                  (+ 1 (monte-carlo-sampling (- n 1))))
                 (else
                  (monte-carlo-sampling (- n 1))))))))
(define (pi-approx n)
  (* 4.0 (/ (monte-carlo-sampling n) n)))

(display "(pi-approx 100000)  = ")
(pi-approx 100000)

(display "(pi-approx 1000000) = ")
(pi-approx 1000000)

(display "(pi-approx 1500000) = ")
(pi-approx 1500000)


