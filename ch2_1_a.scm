
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

				   
(define (make-interval a b) (cons a b))
;####################################################################
;2.7

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;(display (add-interval (make-interval 1 2 ) (make-interval 3 4)))
;(newline)

;2.8
(define (sub-interval x y)
	(make-interval (- (lower-bound x) (lower-bound y))
				   (- (upper-bound x) (upper-bound y))))
				   

;(display (sub-interval (make-interval 1 2 ) (make-interval 3 4)))
;(newline)

;2.9
(define (width x) 
	(/ (- (upper-bound x)(lower-bound x)) 2))

(define i1 (make-interval 1 2))
(define i2 (make-interval -3 5))
(define i3 (make-interval -7 -11))


;(display (width (add-interval i1 i2)))
;(newline)
;(display (+ (width i1) (width i2)))
;(newline)
;(display (width (sub-interval i1 i2)))
;(newline)
;(display (- (width i1) (width i2)))
;(newline)
;(display (width (mul-interval i1 i2)))
;(newline)
;(display (* (width i1) (width i2)))
;(newline)
;(display (width (div-interval i1 i2)))
;(newline)
;(display (/ (width i1) (width i2)))
;(newline)

;2.10
(define (div-interval x y)
	(if (and (< 0 (lower-bound y)) (> 0 (upper-bound y))) 
		(display "undefined behavior") 
		(mul-interval x
			(make-interval ( / 1.0 (upper-bound y))
						   ( / 1.0 (lower-bound y))))))
;(display 
;	(div-interval i1 i2))


;2.11
;(display (mul-interval i1 i1))
;(newline)
;(display (mul-interval i1 i2))
;(newline)
;(display (mul-interval i1 i3))
;(newline)
;(display (mul-interval i2 i1))
;(newline)
;(display (mul-interval i2 i2))
;(newline)
;(display (mul-interval i2 i3))
;(newline)
;(display (mul-interval i3 i1))
;(newline)
;(display (mul-interval i3 i2))
;(newline)
;(display (mul-interval i3 i3))
;(newline)
;(newline)
;(define mul-interval-using-maxmin mul-interval)
(define (mul-interval x y)
		(let ((lx (lower-bound x))
			 (ux (upper-bound x))
			 (ly (lower-bound y))
			 (uy (upper-bound y)))
		(cond 	((and (< 0 lx) (< 0 ux) (< 0 ly) (< 0 uy))
					(make-interval (* lx ly)(* ux uy)))
				((and (> 0 lx) (< 0 ux) (< 0 ly) (< 0 uy))
					(make-interval (* lx uy)(* ux uy)))
				((and (> 0 lx) (> 0 ux) (< 0 ly) (< 0 uy))
					(mul-interval-using-maxmin x y))
				((and (< 0 lx) (< 0 ux) (> 0 ly) (< 0 uy))
					(make-interval (* ux ly)(* ux uy)))
				((and (> 0 lx) (< 0 ux) (> 0 ly) (< 0 uy))
					(mul-interval-using-maxmin x y))
				((and (> 0 lx) (> 0 ux) (> 0 ly) (< 0 uy))
					(make-interval (* ux uy)(* ux ly)))
				((and (< 0 lx) (< 0 ux) (> 0 ly) (> 0 uy))
					(make-interval (* ux uy)(* lx ly)))
				((and (> 0 lx) (< 0 ux) (> 0 ly) (> 0 uy))
					(make-interval (* ux uy)(* lx uy)))
				((and (> 0 lx) (> 0 ux) (> 0 ly) (> 0 uy))	
					(make-interval (* lx ly)(* ux uy)))
				)))

		
;(display (mul-interval i1 i1))
;(newline)
;(display (mul-interval i1 i2))
;(newline)
;(display (mul-interval i1 i3))
;(newline)
;(display (mul-interval i2 i1))
;(newline)
;(display (mul-interval i2 i2))
;(newline)
;(display (mul-interval i2 i3))
;(newline)
;(display (mul-interval i3 i1))
;(newline)
;(display (mul-interval i3 i2))
;(newline)
;(display (mul-interval i3 i3))
;(newline)

;2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
;########################################

(define (make-center-percent c r)
	(make-center-width c (* c (/ r 100))))

(define (percent x)
	(* (/ (width x) (center x)) 100))
;(display (make-center-percent 200 1))
;(display (percent (make-center-percent 200 1)))

;2.13
;(define i4 (mul-interval (make-center-percent 100 0.1) (make-center-percent 2000 0.2)))
;(display (percent i4))
;(newline)
;(display (center i4))
;(newline)
;(define (mul-interval x y)
;		(make-center-percent (* (center x)(center y))
;							 (+ (percent x)(percent y))))
;(define i4 (mul-interval (make-center-percent 100 0.1) (make-center-percent 2000 0.2)))
;(display (percent i4))
;(newline)
;(display (center i4))
;(newline)

;2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
        (add-interval r1 r2)))
(define (par2 r1 r2)
 (let ((one (make-interval 1 1))) 
  (div-interval one
    (add-interval (div-interval one r1)
                  (div-interval one r2)))))
;##################################################
(define A (make-center-percent 100 0.1))
(define B (make-center-percent 50 0.25))
;(display (par1 A B))
;(newline)
;(display (par2 A B))
;(newline)
;(display A)
;(newline)
;(display (percent (div-interval A A)))
;(newline)
;(display (percent (div-interval A B)))

