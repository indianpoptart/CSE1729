"Lab 9"
"Nikhil Paranjape"
"2018-11-28"

;Problem 1
(display "\nProblem 1\n")
;Evaluates to the number of times the symbol appears in the list

(define num-occurs
  (lambda (sym list)
    (cond
      ((null? list) 0)
      ((eq? sym (car list))
       (+ 1 (num-occurs sym (cdr list))))
      (else
       (num-occurs sym (cdr list))))))

(display "(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh)) = ")
(num-occurs 'uh-huh '(thats the way uh-huh uh-huh i like it uh-huh uh-huh))

(display "(num-occurs 'a '(a b c (not (c b a)))) = ")
(num-occurs 'a '(a b c (not (c b a))))

;Problem 2
(display "\nProblem 2\n")
(define freq-list
  (lambda (L)
    (let* ((e (car L))
          (n (num-occurs e L))
          (L (remove-all e L)))
      (cons e n))))