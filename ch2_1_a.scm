
(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
				  (+ (upper-bound x) (upper-bound y))))
				  
(define (mul-interval x y)
	(let ((p1 (* (lower-bound x) (lower-bound y)))
		  (p2 (* (lower-bound x) (upper-bound y)))
		  (p3 (* (upper-bound x) (lower-bound y)))
		  (p4 (* (upper-bound x) (upper-bound y))))
		(make-interval (min p1 p2 p3 p4)
						(max p1 p2 p3 p4))))
						
(define (div-interval x y)
	(mul-interval x
		(make-interval ( / 1.0 (upper-bound y))
					   ( / 1.0 (lower-bound y)))))
(define (div-interval x y)
	(mul-interval x
		(make-interval ( / 1.0 (upper-bound y))
					   ( / 1.0 (lower-bound y)))))
				   
(define (make-interval a b) (cons a b))

;2.8

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;display

(display (add-interval (make-interval 1 2 ) (make-interval 3 4)))
(newline)
;2.8
(define (sub-interval x y)
	(make-interval (- (lower-bound x) (lower-bound y))
				   (- (upper-bound x) (upper-bound y))))
				   
;display
(display (sub-interval (make-interval 1 2 ) (make-interval 3 4)))
(newline)
;2.9
(define (width x) 
	(- (upper-bound x)(lower-bound x)))


(newline)
(newline)
(define i1 (make-interval 1 2))
(define i2 (make-interval 3 4))

(display (width (add-interval i1 i2)))
(newline)
(display (+ (width i1) (width i2)))
(newline)
(display (width (sub-interval i1 i2)))
(newline)
(display (- (width i1) (width i2)))
(newline)
(display (width (mul-interval i1 i2)))
(newline)
(display (* (width i1) (width i2)))
(newline)
(display (width (div-interval i1 i2)))
(newline)
(display (/ (width i1) (width i2)))
(newline)

;2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
        (add-interval r1 r2)))
(define (par2 r1 r2)
 (let ((one (make-interval 1 1))) 
  (div-interval one
    (add-interval (div-interval one r1)
                  (div-interval one r2)))))

(display (par1 i1 i2))
(newline)
(display (par2 i1 i2))
(newline)