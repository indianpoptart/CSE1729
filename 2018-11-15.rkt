(define num-occurs
  (lambda (sym list)
    (cond
      ((null? list) 0)
      ((eq? sym (car list))
       (+ 1 (num-occurs sym (cdr list))))
      (else
       (num-occurs sym (cdr list))))))

(define (remove-all x L)
  (cond ((null? L) L)
        ((list? (car L))
         (cons (remove-all x (car L)) (remove-all x (cdr L))))
        ((equal? x (car L)) (remove-all x (cdr L)))
        (else (cons (car L) (remove-all x (cdr L))))))

(define freq-list
  (lambda (L)
    (let* ((e (car L))
          (n (num-occurs e L))
          (L (remove-all e L)))
      (cons e n))))
      

(let* ((e (car L))
      