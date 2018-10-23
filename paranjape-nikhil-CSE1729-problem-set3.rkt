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
      (+ (* (expt -1 n)
            (/ (expt x (+ 1 (* 2 n)))
               (factorial (+ 1 (* 2.0 n)))))
         (new-cos x (- n 1)))))

(new-cos 1 3)

;Problem 2
(display "\nProblem 2\n")

(define (harmonic n)
  (cond ((<= n 1) 1)
        (else (+ (/ 1 n) (harmonic (- n 1))))))

(define (euler-approx n)
  (abs (- (harmonic n) (log n))))

(euler-approx 3)

;Problem 3a
(display "\nProblem 3a\n")

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond
    ((= count 0) b)
    ((even? count)
     (fib-iter a
               b

;Problem 4a
(define (golden n)
  (cond
    ((= n 1) 2)
    (else
     (+ 1.0 (/ 1.0 (golden (- n 1)))))))

(display "\n(golden 6) = ")
(golden 6)

;Problem 4b
(define (golden-approx n d k)
  (define (iter i res)
    (if (zero? i)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter (- k 1) (/ (n k) (d k))))

(display "(golden-approx 1.0 1 6) = ")
(+ 1.0 (golden-approx (lambda (x) 1) (lambda (x) 1) 6))