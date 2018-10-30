(define (insert element set)
  (cons element set))

(define (ismember? element set)
  (cond ((null? set ) #f)
        ((equal? element (car set)) #t)
        (else (ismember? element (cdr set)))))


(define (make-tree value left right)
  (list value left right))

(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))

(define (element? x T)
  (cond ((null? T) #f)
        ((eq? x (value T)) #t)
        ((< x (value T)) (element? x (left