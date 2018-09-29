"Lab 4"
"Nikhil Paranjape"
"2018-09-29"

;Partner:null(Klaus was like nah)

(newline)

(display "Problem 1\n")
;Calculate harmonic sum
(define (harmonic n)
  (cond
    ((= 1 n) 1.0)
    (else
     (+ (/ 1 n) (harmonic (- n 1))))))

"(harmonic 3)"
(harmonic 3)

"(harmonic 6)"
(harmonic 6)

;Example code
(display "
(define (sum f n)
  (if (= n 1)
      (f 1)
      (+ (f n) (sum f (- n 1)))))
")
(define (sum f n)
  (if (= n 1)
      (f 1)
      (+ (f n) (sum f (- n 1)))))

(display "\nProblem 2\n")
;Calculate harmonic sums
(define (harm-term n)
  (/ 1.0 n))

(define (harm-sum n)
  (sum harm-term n))
    
  
(harm-sum 2)

(newline)

(display "\nProblem 3a\n")
;General summation function g-sum

(define (g-sum f a b)
  (cond
    ((= b a) (f a))
    (else
     (+ (f a) (g-sum f (+ a 1) b)))))

"(g-sum harm-term 1 4)"
(g-sum harm-term 1 4)

(display "\nProblem 3b\n")

"(g-sum harm-term 1 4)"
(g-sum harm-term 1 4)

"(g-sum harm-term 3 7)"
(g-sum harm-term 3 7)

(display "\nProblem 3c\n")
;Define a geometric series

(define (geometric-term i)
  (/ 1 (expt 2 i)))


(define (geom-series-np2 n)
  (g-sum geometric-term 0 n))

"(geom-series-np2 3)"
(geom-series-np2 3)

(display "\nProblem 3d\n")
;unnamed function using lambdas

(define (convergent-series a b)
  (g-sum (lambda (k) (/ 1 (* k k))) a b))

"(convergent-series 1 7)"
(convergent-series 1 7)

(display "\nProblem 4a\n")
; A function find

(define (find sequence test n)
  (define (find-helper x found)
    (let* ((fx (sequence x))
           (satisfies-test (test fx)))
      (cond
        ((and satisfies-test (= (+ found 1) n)) fx)
        (satisfies-test (find-helper (+ x 1) (+ found 1)))
        (else
         (find-helper (+ x 1) found)))))
  (find-helper 1 0))

"--- BEGIN HELPER ---"
(display "
(define (find-helper x found)
    (let* ((fx (sequence x))
           (satisfies-test (test fx)))
      (cond
        ((and satisfies-test (= (+ found 1) n)) fx)
        (satisfies-test (find-helper (+ x 1) (+ found 1)))
        (else
         (find-helper (+ x 1) found)))))
  (find-helper 1 0))
")
"--- END HELPER  ---"
(display "\nProblem 4b\n")
; Test find program

(define (even x) (= (modulo x 2) 0))
(define (odd x) (not (even x)))
(define (seq x) x)

"(find seq even 15)"
(find seq even 15)

"(find seq odd 15)"
(find seq odd 15)

(display "\nProblem 4c\n")
(define (fib x)
  (cond
    ((< x 2) 1)
    (else
     (+ (fib (- x 1)) (fib (- x 2))))))
(define (divides a b) (= (modulo b a) 0))
(define (smooth n k)
  (and (>= k 2)
       (or (divides k n)
           (smooth n (- k 1)))))
(define (isprime p)
  (and (> p 1)
       (not (smooth p (floor (sqrt p))))))

"(find fib isprime 5)"
(find fib isprime 5)

(display "\nProblem 5a\n")
;(f o g)(x) = (f(g(x))

(define (comp f g)
  (lambda (x)
    (f (g x))))

"(define (double x) (* 2 x))"
(define (double x) (* 2 x))

"(define (add-one x) (+ x 1))"
(define (add-one x) (+ x 1))

"(define com (comp add-one double))"
(define com (comp add-one double))

"(com 3)"
(com 3)

"((comp double add-one) 3)"
((comp double add-one) 3)