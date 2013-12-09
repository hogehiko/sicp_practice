;2.33
(define nil '())
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
	(accumulate op initial ( cdr sequence)))))

;(print (accumulate * 1 (list 1 2 3)))

;2.33
;(define (map p sequence)
;  (accumulate (lambda (x y)
;		(cons (p x) y)) 
;	      nil 
;	      sequence))

;(print (map (lambda (x) (* x 2)) (list 1 2 3)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

;(print (append (list 1 2 3) (list 4 5 6)))


(define (length sequence)
  (accumulate (lambda (e folded) (+ folded 1)) 0 sequence))

;(print (length (list 100 100 100)))

;2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
		(+ (* higher-terms x) this-coeff))
	      0
	      coefficient-sequence))

;(print (horner-eval 2 (list 1 1 1 )))


;2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (e)
			 (if (pair? e)
			   (count-leaves e)
			   1))
		       t )))
;enumulate-treeを使って実装する
;(print (count-leaves (list (list 1 2) (list 3 4) 5)))

;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
	  (accumulate-n op init (map cdr seqs)))))

;(print (accumulate-n  + 0 (list (list 1 2 3) (list 4 5 6))))


;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define e (list (list 1 1) (list 1 1)))

;(print (dot-product (list 1 1) (list 2 3)))

(define (matrix-*-vector m v)
  (map (lambda (e) (dot-product v e)) m))

;(print (matrix-*-vector e (list 2 3)))
(define (transpose mat)
  (accumulate-n cons nil mat))

;(print (transpose (list (list 1 2) (list 3 4))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (e) (matrix-*-vector cols e))m )))

;(print (matrix-*-matrix e e))


;2.38
;(print (fold-right / 1 (list 1 2 3)))
;3/1
;2/(3/1)
;1/(2/(3/1))
;(print (fold-right / 1 (list 1 2 3)))
;(print (fold-left / 1 (list 1 2 3)))
;opは交換法則を満たす
;initialが単位元なら結合法則
;2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

;(print (reverse (list 1 2 3)))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;(print (reverse (list 1 2 3)))
