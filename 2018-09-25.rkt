(define (divides a b) (= (modulo b a) 0))
(define (smooth n k)
  (and (>= k 2)
      (or (divides k n)
          (smooth n (- k 1)))))
(define (composite n) (smooth n(floor (sqrt n))))

(define (ceil n)
  (ceiling n))

(define (isprime p)
  (not (smooth p (floor (sqrt p)))))

;(isprime 1041291041298831838388318383) test on TR
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (sin-term t)
  (if (odd? t)
      (/ (expt -1 (/ (- t 1) 2)) (fact t))
      0))

(define (power-series x term k)
  (if (< k 0)
      0
      (+ (* (term k) (expt x k))
         (power-series x term (- k 1)))))

;(power-series 0.5 sin-term 100000)