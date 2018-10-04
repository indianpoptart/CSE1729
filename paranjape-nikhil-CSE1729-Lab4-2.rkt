"Lab 4"
"Nikhil Paranjape"
"2018-09-30"

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

(define (is-prime x)
  (cond
    ((<= x 1) #f)
    ((<= x 3) #t)
    ((modulo x 3) #t)
    (else
     (+ 1 (is-prime (- x 1))))))

(display "\nProblem 4a\n")
(define (find sequence test n)
  (cond
    ((= sequence (harmonic n)) #f)
    (else
     (+ 1 1))))
  
  