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