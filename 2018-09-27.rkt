(define (sum f n)
  (if (= n 0)
      (f 0)
      (+ (f n) (sum f (- n 1)))))


(define (f x)
  (define (g y) (+ x y))
  (define (h x) (+ x (g x)))
  (h (+ x 10)))
(f 100)

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fact-accumulate n a)
  (if (= n 0) a
      (fact-accumulate (- n 1)
                       (* n a))))