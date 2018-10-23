(define (fast-fib n)
  (define (fib-pair n)
    (if (= n 1)
        (cons 1 0)
        (let ((prev-pair (fib-pair (- n 1))))
          (cons (+ (car prev-pair)
                   (cdr prev-pair))
                (car prev-pair)))))
  (car (fib-pair n)))

(define (slow-fib n)
  (cond
    ((= n 0) 0)
    ((= n 1) 1)
    (else
     (+ (slow-fib (- n 1)) (slow-fib (- n 2))))))

(define (make-rat a b)
  (let ((d (gcd a b)))
  (cons (/ a d) (/ b d))))

(define (denom r) (cdr r))
(define (numer r) (car r))

(define (mult-rat r s)
  (make-rat (* (numer r) (numer s))
            (* (denom r) (denom s))))