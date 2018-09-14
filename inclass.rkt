(define (fmult a b)
  (cond ((= a 0) 0)
        ((= b 0) 0)
        ((<= a b) (+ b (fmult (- a 1) b)))
        ((> a b) (+ a (fmult a (- b 1))))))

(define (even x) (= (modulo x 2) 0))
(define (twice x) (* x 2))
(define (half x) (/ x 2))

(define (rfmult a b)
  (cond ((= 0 a) 0)
        ((= 0 b) 0)
        ((even a ) (twice (rfmult (half a) b)))
        (else      (+ b (twice (rfmult (half (- a 1))
                                             b))))))

