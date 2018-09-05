(define (today x)
  (cond
    ((= x 1) "one")
    ((= x 2) "two")
    ((= x 3) "three")
    (#t "Something Else")))

(define (absolute x)
        (if (>= x 0)
            x
            (* -1 x)))

(define (hailstone n)
        (if (= (modulo n 2) 0)
            (/ n 2)
            (+ (* 3 n) 1)))

(define dayname "Tuesday")
(define date 4)
(define (add2 x) (+ x 2))
(define (add5 y) (+ y 5))

(define (roll-over daynumber monthlength)
  (if (> daynumber monthlength)
      (- daynumber monthlength)
      daynumber))

(roll-over
 ((if (eq? dayname "Tuesday")
      add2
      add5)
  date)
 30)

(define (square x) (* x x))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
