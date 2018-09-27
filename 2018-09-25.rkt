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
