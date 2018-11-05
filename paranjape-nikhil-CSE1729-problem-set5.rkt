"Problem Set 5"
"Nikhil Paranjape"
"2018-11-05"

;Problem 1a
(display "\nProblem 1a\n")

(define is-prefix?
  (lambda (L1 L2)
    (cond
      ((null? L1) #t)
      ((null? L2) #f)
      ((= (car L1)(car L2))
       (is-prefix? (cdr L1)(cdr L2)))
      (else
       #f))))

(is-prefix? '(1) '(1 2 3))

;Problem 1b
(display "\nProblem 1b\n")

(define longest-common-prefix
  (lambda (L1 L2)
    (cond
      ((null? L1) '())
      ((null? L2) '())
      ((= (car L1)(car L2))
       (cons (car L1)(longest-common-prefix (cdr L1)(cdr L2))))
      (else '()))))

(longest-common-prefix '(1 2) '(1 2 3))

;Problem 2a
(display "\nProblem 2a\n")

(define (list-gen from to)
   (if (> from to)
      '()
      (cons from (list-gen (+ from 1) to))))

(define list-gen
  (lambda (a b)
      (if (> a b)
         '()
         (cons a (list-gen (+ a 1) b)))))

(define (gen-consecutive f a b)
  (map f (list-gen a b)))

(gen-consecutive (lambda(x)(* x x)) 1 10)

;Problem 2b
(display "\nProblem 2b\n")

(define gen-sequence
  (lambda (f a b next)
    (if (> a b)
        '()
        (cons (f a a) (gen-sequence f (next a) b next)))))

(define factorial
  (lambda (n acc)
     (if( = n 0 ) 
        acc
      (factorial (- n 1)  (* acc n )))))

(gen-sequence factorial 1 5 (lambda(x)(+ x 2)))

;Problem 3
(display "\nProblem 3\n")

(define filter
  (lambda (f L1)
    (cond
      ((null? L1) '())
      ((f (car L1))
       (cons (car L1)(filter f (cdr L1))))
      (else
       (filter f (cdr L1))))))

(display "\n(filter even? '(1 2 3 4 5 6 7)) = ")
(filter even? '(1 2 3 4 5 6 7))

;Problem 4a
(display "\nProblem 4a\n")

(define value-at-position
  (lambda (L1 k)
    (if (= k 1)
        (car L1)
        (value-at-position (cdr L1 (- n 1))))))

;Problem 4b
(display "\nProblem 4b\n")

(define (is-prime? n)
  (let loop ((d 2))
    (cond ((< n (* d d)) #t)
          ((zero? (modulo n d)) #f)
          (else (loop (+ d 1))))))

(define nth-prime-between
  (lambda (a b n)
    (value-at-position (filter is-prime? (consecutive-ints a b)) n)))
    