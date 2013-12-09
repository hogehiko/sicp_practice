(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))



(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))

(define nil '())

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flat-map
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-queen row col)
  (cons row col))

(define (adjoin-position col row rest-of-queens)
  (cons (make-queen row col) rest-of-queens))

(define empty-board nil)
(define (row l)
  (car l ))
(define (col l)
  (cdr l ))
;(print (col (make-queen 1 2)))
(define b (adjoin-position 2 2(adjoin-position  1 2 empty-board)))
(define (ng-positions k board)
  (flat-map (lambda (each) (list (col each)
			       (- (col each)(distance each k))
			       (+ (col each)(distance each k))))
	    board))
(define (distance p k)
  (- k (row p)))
(define (in_array needle l)
  (not (eq? 0
            (accumulate + 
			0 
			(map (lambda (e)
			       (if (= e needle) 1 0)) l)))))


(define (safe? k board)
   (not (in_array (col (car board)) (ng-positions k (cdr board)))))
;(print (in_array 1 (list 4 2 3)))
(print (queens 4))
