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
(newline)
"(f 2)"
(f 2)
(newline)
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

" --- Begin Helper --- "
(display "( define ( my-substr s)
   ( cond
   (( string=? s ` `'') `` '')
   ( else
    ( substring s 1 ( string-length s )))))
")
" --- End Helper --- "

(newline)

( define ( my-substr s)
   ( cond
      ((string=? s "") "")
      ( else
        ( substring s 1 ( string-length s )))))

(define (my-str-len s)
  (+ 1 (string-length (my-substr s))))

(display "(my-str-len 'test')\n")
(my-str-len "test")

(display "(my-str-len 'racket')\n")
(my-str-len "racket")

(display "\nProblem 2b\n")
;A function that checks if something is a substring of something else

(define (substring? sstr main-str)
  (cond
    ((string=? main-str "") "")
    ((string=? sstr "") "")
    ((string-ci=? main-str sstr) #t)
    ((string-ci=? sstr (substring main-str 0 (string-length sstr))) #t)
    (else
     #f)))

"(substring? ''test'' ''testing'')"
(substring? "test" "testing")
(newline)
"(substring? ''comp'' ''Computer science'')"
(substring? "comp" "Computer science")
(newline)
"(substring? ''biology'' ''science'')"
(substring? "biology" "science")

(display "\nProblem 2c\n")
; A function that reverses strings

" --- Begin Helper --- "
(display "(define (substr-ref str n) (substring str n (+ n 1)))\n")
" --- End Helper --- "

(define (substr-ref str n) (substring str n (+ n 1)))

;Not sure if we are allowed to use trees yet
;From page 159 in the book 
(define (string-reverse in-string)
  (list->string
   (reverse
    (string->list in-string))))

"(string-reverse 'racecar')"
(string-reverse "racecar")
(newline)
"(string-reverse 'uconn')"
(string-reverse "uconn")
(newline)
"(string-reverse 'laboratory')"
(string-reverse "laboratory")


(display "\nProblem 3a\n")
;Binaryplus takes two args and adds them
(define (binary-plus a)
  (lambda (x)
    (+ a x)))

(define plus5 (binary-plus 5))

"(plus5 7)"
(plus5 7)
"(plus5 8)"
(plus5 8)
"(plus5 9)"
(plus5 9)

(display "\nProblem 3b\n")
(define (trinary-plus a)
  (lambda (x)
    (lambda (y)
      (+ a x y))))

(define plus5+4 ((trinary-plus 5) 4))

"(plus5+4 6)"
(plus5+4 6)
"(plus5+4 7)"
(plus5+4 7)
"(plus5+4 8)"
(plus5+4 8)