
;2.26
(define (reverse l)
  (define (iter remain tail)
    (if (null? remain)
      tail
      (iter (cdr remain) 
	    (cons (if (pair? (car remain))
		    (reverse (car remain))
		    (car remain)) 
		  tail))))
  (iter l ()))
		

;(print (reverse (list 1 (list 4 5) 3)))

(define (fringe l)
  (append (if (pair? (car l))
	    (fringe (car l))
	    (list (car l))) 
	  (if (pair? (cdr l))
	    (fringe (cdr l))
	    (cdr l))))

;apply‚Åˆø”‚É˜A‘±“K—p‚Å‚«‚é


(define l (list 1 2))

(define nil '())
(define (map f l)
  (if (pair? l)
    (cons (f (car l))
	  (map f (cdr l)))
    nil))
	  

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) (cons ( car s) x))
			rest)))))
(print (subsets (list 1 2 3)))
