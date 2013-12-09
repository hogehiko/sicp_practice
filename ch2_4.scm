(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;type tag
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tgged datum -- TPYE=TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum -- CONTENTS" datum)))

;rectangular implementation
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define ( magnitude z)
    (sqrt (+ (square (real-part z))
  	     (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)
	  (real-part z)))

  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang x y)
    (cons (* r (cos a)) (* r(sin a))))
  
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) ( tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)(tag (make-from-man-ang r a))))
  'done)
;poler implementation
(define (install-polar-package)

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (make-from-real-imag-polar x y)
    (cons (sqrt (+ (square x)(square y)))
		      (atan y x)))
  (define (make-from-mag-ang-polar r a)
    (cons r a))

  (define (tag x) ( attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)(tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)(tag (make-from-mag-ang))))
  'done)

(define (apply-generic op . args)
  (let ((type-tag (map type-tag args)))
    (if proc
      (apply proc (map contents args))
      (error
	"No method for these types -- APPLY-GENERIC"
	(list op type-tags)))))


