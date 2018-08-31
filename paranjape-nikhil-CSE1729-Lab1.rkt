"Lab 1"
"Nikhil Paranjape"
"2018-09-07"
(newline)

"problem 1a"
;USD to GBP"
(define (usd-gbp x)
  (display "£")
  (* x 0.752))

"(usd-gbp $175)"
(usd-gbp 175)

"problem 1b"
;GBP to Euro
(define (gbp-euro z)
  (display "€")
  (* z 1.192))

"(gbp-euro £20)"
(gbp-euro 20)

"problem 1c"
;Euro to Krona
(define (euro-krona e)
  (display "kr")
  (* e 9.58))

"(euro-krona €240)"
(euro-krona 240)

"problem 1d"
;USD to Krona
(define (usd-krona f)
  (display "kr")
  (euro-krona
   (gbp-euro
     (usd-gbp f))))
;TODO add conversion rate

"problem 2a"
(define (det2x2 a b
                c d)
  (- (* a d) (* b c)))

"(det2x2 -3 1 2 7)"
(det2x2 -3 1 2 7)

"problem 2b"
(define (invertible a b
                    c d)
  (not (= (det2x2 a b c d) 0)))

(display "Is N invertible?\n")

"(invertible -3 1 2 7)"
(invertible -3 1 2 7)

(display "Is M invertible?\n")

"(invertible 2 -4 -6 12)"
(invertible 2 -4 -6 12)


"problem 2c"
(define (detMult a1 b1 c1 d1 a2 b2 c2 d2)
  (invertible (+ (* a1 a2) (* b1 c2))
               (+ (* a1 b2) (* b1 d2))
               (+ (* c1 a2) (* d1 c2))
               (+ (* c1 b2) (* d1 d2))))

"problem 2d"
(define (det3x3 a b c
                d e f
                g h i)
  (+ (- (* a (det2x2 e f h i))
        (* b (det2x2 d f g i)))
        (* c (det2x2 d e g h))))
(display "                            _______\n")
(display "                           |0  5 -6|\n")
(display "What is the determinant of |8 -11 4| ?\n")
(display "                           |5  1  1|\n")
(display "                            ̅̅̅̅̅̅̅\n")

(display "       0  5 -6\n")
(display "det3x3 8 -11 4\n")
(display "       5  1  1\n")

(det3x3 0 5 -6
        8 -11 4
        5 1 1)

  