(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(define p2l (list 3))
(define p2 (cons 1 (cons p2l p2l)))

(print (count-pairs p2))

(define p3l (list 3))
(define p3l2 (cons p3l p3l))
(define p3 (cons p3l2 p3l2))
(print p3)
(print (count-pairs p3))
