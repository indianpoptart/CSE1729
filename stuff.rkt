(define factorial
  (lambda (n)
    (cond
      ((= n 0) 1)
      (else
       (* n (factorial (- n 1)))))))

(define (fast-fib n)
  (define (fib-pair n)
    (if (= n 1)
        (cons 1 0)
        (let ((prev-pair (fib-pair (- n 1))))
          (cons (+ (car prev-pair)
                   (cdr prev-pair))
                (car prev-pair)))))
  (car (fib-pair n)))

(/ (fast-fib 364) (* (factorial 342) (expt 365 22)))