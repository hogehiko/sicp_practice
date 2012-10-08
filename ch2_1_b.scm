(define zero (
	lambda (f) (
		lambda (x) x
	)))

(define (add-1 n)
  (lambda (f) 
	(lambda (x) 
		(f ((n f) x)))))
  
;te
;zero
;lambda (f) (lambda (x) x)
;
;(lambda (f) 
;	(lambda (x) (f (
;		((lambda (f) (lambda (x) x)) f);change -> (lambda (x) x)
;	x))))
;
;(lambda (f) 
;	(lambda (x) (f (
;		(lambda (x) x) x)
;	)))
;(lambda (f) (lambda (x) (f x)))
	
(define one (add-1 zero))
(define two (add-1 one))
 
(define (add m n) 
	(lambda (f) (
		lambda (x)(
			(m f)((n f) x)
		)
	)))
	
(define (mul m n) 
	(lambda (f) (
		lambda (x)(
			(m (n f)) x
		)
	)))
(define (inc n) (+ n 1))
;(display (((add-1 zero) inc) 0))
(display (((add two two) inc) 0))
(display (((mul two (add one two)) inc) 0))