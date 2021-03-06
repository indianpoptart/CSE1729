(define last-both
  (lambda (l1 l2)
    (cons (car (reverse l1))
          (car (reverse l2)))))

(define powers-of-2
  (lambda (n)
    (cond
      ((= 0 n) '(1))
      (else
       (append (powers-of-2 (- n 1)) (list (expt 2 n)))))))


(define gen-list
  (lambda (x y z)
    (cond
      ((> x y) '())
      (else
       (cons (z x) (gen-list (+ x 1) y z))))))

(define dot-product
  (lambda (l1 l2)
    (cond
      ((or (null? l1) (null? l2)) 0)
      (else
       (+ (* (car l1) (car l2)) (dot-product (cdr l1) (cdr l2)))))))

(define function
  (lambda (n)
    (cond
      ((<= n 2) 1)
      (else
       (+ (function (/ n 2)) (function (/ n 2)) n)))))

(define bst-max
  (lambda (bst)
    (if (null? (cdr bst))
        (car bst)
        (bst-max (cdr bst)))
    (bst-max bst)))

(let*
   (( g +)
    (f ( lambda (x ) (g x 5)) ) )
   ( map f '(1 2 3 4 5) ))

(define x 4)

(define y 5)

(let ((x 1)
      (y 2))
  (* x y))

(* x y)

(define enqueue
  (lambda (x Q)
    (if (null? Q)
        (list x)
        (cons (car q)
              (enqueue x (cdr q))))))

(define (front Q) (car Q))

(define (empty? Q) (null? Q))

(define (dequeue Q) (cdr Q))

(define enqueue-cond
  (lambda (x Q)
    (cond
      ((null? Q) (list x))
      (else
       (cons (car Q)
             (enqueue x (cdr Q)))))))

(define even-numbers
  (lambda (L)
    (cond
      ((null? L) '())
      ((null? (cdr L)) '()
                       (list (car L))
                       (cons (cadr L)
                             (even-numbers (cddr L)))))))

(let
    ((x 1))
  (let
      ((x -5))
    (* x x)))

(define x 10)
(let
    ((x (+ 1 x))
     (y 5))
  (let
      ((x (+ 1 x))
       (y (+ 10 y)))
    (* x y)))

(define (sum-even-ints n)
  (cond
    ((= n 0) 0)
    ((odd? n) (sum-even-ints (- n 1)))
    (else
     (+ n (sum-even-ints (- n 1))))))


(cons '(the) (cons '(a) '(magnificent)))


(define (mystery n)
  (define (enigma a b count)
    (cond
      ((= count 0) b)
      (else
       (enigma (+ a b) a (- count 1)))))
  (enigma 1 0 n))

(define my-map
  (lambda (f L1 L2)
    (cond
      ((null? L1) '())
      (else
       (cons (f (car L1) (car L2))
             (my-map f (cdr L1) (cdr L2)))))))

((lambda (a b) (+ a b 99)) 10 20)

((lambda (a)
   ((lambda (b) (+ a b 99)) (+ a 20))) 10)

(define msum
  (lambda (n)
    (cond
      ((= n 0) 1)
      (else
       (+ 1 (* 2 n) (msum (- n 1)))))))

(define every-second-element
  (lambda (L1)
    (cond
      ((null? L1) '())
      ((null? (cdr L1)) (list (car L1)))
      (else
       (cons (car L1) (every-second-element (cddr L1)))))))