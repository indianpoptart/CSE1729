;;
;;
;; Equivalence of let* and lambdas
;;
;; PGB 11/7/2018
;;

(define x -1)

((lambda (a)
         ((lambda (b)
            ((lambda (c)
               (+ a b c)) (+ x 7 a)))
          (+ a 1))
         ) 3)
          
(let*
    ((a 3)
     (b (+ a 1))
     (c (+ x 7 a)))
  (+ a b c))

(define msum
  (lambda (k)
    (cond
      ((= k 0) 1)
      (else
       (+ 1 (* 2 k) (msum (- k 1)))))))

(define scale-list
  (lambda (items factor)
    (cond
      ((null? items) '())
      (else
       (cons (* (car items) factor)
             (scale-list (cdr items) factor))))))

(define accumulate
  (lambda (op initial sequence)
    (cond
      ((null? sequence) initial)
      (else
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))