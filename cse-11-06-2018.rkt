(define x -1)

((lambda (a)
  ((lambda (b)
    ((lambda (c)
       (+ a b c)) (+ x 7 a)))
     (+ a 1))) 3)

(let* ((a 3)
      (b (+ a 1))
      (c (+ x 7 a)))
  (+ a b c))
