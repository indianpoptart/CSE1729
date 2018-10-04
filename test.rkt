(define (a m n)
    (cond
      ((= m 0) (+ n 1))
      ((= n 0) (a (- m 1) n))
      (else
       (a (- m 1) (a m (- n 1))))))

(a 0 1)

(a 1 2)

(a 3 4)