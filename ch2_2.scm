

;2.17
(define nil '())
(list 1 2 3 4)

(define (last-pair l)
		(if (= 0 (length (cdr l)))
			l
			(last-pair (cdr l))))
(display (last-pair (list 1 2 3 4)))

;2.18
(define (reverse l)
		(if (= 0 (length l))
			(l)
			(append  (reverse (cdr l)) (list (car l)))))
;TODO asobu
(define (reverse l)
		(if (null? l)
			nil
			(cons (cdr l) (reverse (car l)))))
			
;(display (reverse (list 1 2 3 4)))

;2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? c)
	(null? c))
	
(define (except-first-denomination  c)
	(cdr c))

(define (first-denomination  c)
	(car c))
	
;(display (cc 100 us-coins))

;2.20
(define (is_even x) (if (= 0 (modulo x 2)) 1 0))
(define (same-parity  . numbers )
	(define (filter head l)
		(if (= 0 (length l))
			nil
			(if  (= (is_even head) (is_even (car l)))
			(cons (car l) (filter head (cdr l)))
			(filter head (cdr l)))	
			))
	 (cons (car numbers) (filter (car numbers) (cdr numbers))))
;(display (same-parity  1 2 3 4 5))
;(newline)
;(display (same-parity  2 3 4 5 6))

;2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items)(car items)) (square-list (cdr items)))))
;(display (square-list (list 1 2 3)))
(define (square-list items)
  (map (lambda x (* (car x) (car x))) items))
;(display (square-list (list 1 2 3)))

(define (for-each p l)
	(if (null? l)
		nil
		((p (car l))
		 (for-each p (cdr l))
		 nil)))

;(for-each (lambda (x) (newline)(display x))
;	(list 57 321 88))

(define (reverse l)
  (define (iter remain tail)
    (if (null? remain)
      tail
      (iter (cdr remain) (cons (car remain) tail))))
  (iter l ()))
		

(print (reverse (list 1 2 3)))
