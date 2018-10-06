"Lab 4"
"Nikhil Paranjape"
"2018-10-11"

(newline)

(display "Problem 1\n")
;       / 1                  if n = 1
;f(n) =<                                      
;       \ f(n-1)*f(n-1)+2n+1 otherwise  

(define (f n)
  ((lambda (x)(cond
                ((= n 1) 1)
                (else
                 (+ (*
                     (expt (f (- n 1)) 2))
                     (* 2 n)
                     1)))) n))

"(f 1)"
(f 1)

"(f 2)"
(f 2)

"(f 3)"
(f 3)

;1. (string-length abc) produces 3
;2. (string-length ) produces 0
;3. (substring hello world 0 5) produces hello
;4. (string=? hello good bye) produces #f
;5. (string-ci=? hello HELLO) produces #t
;6. (string-append Hello   Goodness) produces "Hello Goodness"


(display "\nProblem 2a\n")
;Calculates string length
;Must use (my-substr)

( define ( my-substr s)
   ( cond
      (( string =? s "") "")
      ( else
        ( substring s 1 ( string-length s )))))

" --- Begin Helper --- "
(display "( define ( my-substr s)
   ( cond
   (( string =? s ` `'') `` '')
   ( else
    ( substring s 1 ( string-length s )))))
")
" --- End Helper --- "

(define (my-str-len s)
  (cond
    ((= (my-substr s) 0) 0)
    (else
     (