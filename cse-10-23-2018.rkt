(define x 100)

(let*
      ((x -99)
       (y (+ x 1)))
    y)

(let
      ((x -99)
       (y (+ x 1)))
    y)

(define (remove v elements)
  (if (null? elements)
      elements
      (if (equal? v (car elements))
          (cdr elements)
          (cons (car elements)
                (remove v (cdr elements))))))

(define (smallest l)
  (define (smaller a b) (if (< a b) a b))
  (if (null? (cdr l))
      (car l)
      (smaller (car l) (smallest (cdr l)))))

(define (selSort l)
  (if (null? l)
      '()
      (let* ((first (smallest l))
             (rest (remove first l)))
        (cons first(selSort rest)))))