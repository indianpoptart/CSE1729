"Lab 4"
"Nikhil Paranjape"
"2018-09-30"

;Partner:

(newline)

(display "Problem 1\n")
;Calculate harmonic numbers
(define (harmonic n)
  (cond
    ((= 1 n) 1)
    (else
     (+ (/ 1 n) (harmonic (- n 1))))))

"(harmonic 3)"
(harmonic 3)

"(harmonic 6)"
(harmonic 6)

(newline)

;Example code
(define (sum f n)
  (if (- n 1)
      (f 1)
      (+ (f n) (sum f (- n 1)))))

(display "Problem 2\n")
;Calculate harmonic sums
