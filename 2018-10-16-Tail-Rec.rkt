(define (nlen lst)
  (cond
    ((null? lst) 0)
    (else
     (+ 1 (nlen (cdr lst))))))

(define (len-t lst)
  (define (helper lst len-so-far)
    (cond
      ((null? lst) len-so-far)
      (else
       (helper (cdr lst) (+ 1 len-so-far)))))
  (helper lst 0))

(define (msum lst)
  (cond
    ((null? lst) 0)
    (else
     (+ (car lst)
        (msum (cdr lst))))))

(define (sum-t lst)
  (define (helper lst sum-so-far)
    (cond
      ((null? lst) sum-so-far)
      (else
       (helper (cdr lst) (+ (car lst) sum-so-far)))))
  (helper lst 0))


(define (square-list k)
  (cond
    ((= k 0) (list 0))
    (else
     (cons (* k k)
           (square-list (- k 1))))))


(define (squares start finish)
  (define (square x) (* x x))
  (if (> start finish)
      '()
      (cons (square start)
            (squares (+ start 1) finish))))

;
; or use tail-recursion
;
(define (square-list-t k)
  (define (helper k list-so-far)
    (cond
      ((= k 0) (cons k list-so-far))
      (else
       (helper (- k 1)
               (cons (* k k) list-so-far)))))
  (helper k '()))


