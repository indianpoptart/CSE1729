"Lab 3"
"Nikhil Paranjape"
"2018-09-20"

"License: https://github.com/indianpoptart/CSE1729/blob/master/LICENSE"

(newline)

(display "Problem 1a\n")
;A function that computes the number of Jeanie's ancestors in the nth previous generation
;Without using the expt function

(define (pow x y)
  (if (= y 0)
      1
      (if (= y 1)
          x
          (* x (pow x (- y 1))))))


(define (ancestor-compute n)
  (cond
    ((= 0 n) 0)
    (else
     (+ (pow 2 n) (ancestor-compute (- n 1))))))
      
    
  

(ancestor-compute 3)

;TODO Problem 1b


(display "problem 2\n")
;Pell's Numbers

(define (pell-num n)
  (cond
    ((= 0 n) 2)
    ((= 1 n) 2)
    (else
     (+ (* 2 (pell-num (- n 1))) (pell-num (- n 2))))))

"(pell-num 3)"
(pell-num 3)
  