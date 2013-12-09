(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))
 
(define (type-tag datum)
    (if (pair? datum)
      (car datum)
      'scheme-number))
 
(define (contents datum)
    (if (pair? datum)
      (cdr datum)
      datum))
 
(define (square x) (* x x))
(define (install-rectangular-package)
  ; internal procedure
  (define (real-part z)
      (car z))
  (define (imag-part z)
      (cdr z))
  (define (make-from-real-imag x y)
      (cons x y))
  (define (magnitude z)
      (sqrt (+ (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
      (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))
 
  ; public interface
  (define (tag x)
      (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang x y))))
  'done)
 
(define (install-polar-package)
  ; internal procedure
  (define (magnitude z)
      (car z))
  (define (angle z)
      (cdr z))
  (define (make-from-mag-ang r a)
      (cons r a))
  (define (real-part z)
      (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))
 
  (define (tag x)
      (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang x y))))
  'done)
 
 
(define (apply-generic op . args)
  (print args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))
 
(define (real-part z)
    (apply-generic 'real-part z))
(define (imag-part z)
    (apply-generic 'imag-part z))
(define (magnitude z)
    (apply-generic 'magnitude z))
(define (angle z)
    (apply-generic 'angle z))
 
 
(define (make-table)
    (let ((local-table (list '*table*)))
      (define (lookup key-1 key-2)
          (let ((subtable (assoc key-1 (cdr local-table))))
            (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                  (cdr record)
                  false))
                false)))
      (define (insert! key-1 key-2 value)
          (let ((subtable (assoc key-1 (cdr local-table))))
            (if subtable
              (let ((record (assoc key-2 (cdr subtable))))
                (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
              (set-cdr! local-table
                        (cons (list key-1
                                    (cons key-2 value))
                              (cdr local-table)))))
          'ok)
      (define (dispatch m)
          (cond ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation --TABLE" m))))
      dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
 
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
 
(define (install-scheme-number-package)
    (define (tag x)
      (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    (put 'nega '(scheme-number)
         (lambda (x) (tag (- x))))
    'done)
 
(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))
 
(define (install-rational-package)
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g))))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))
 
    ;; interface to reset of the system
    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
 
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    'done)
 
(define (make-rational n d)
  ((get 'make 'rational) n d))
 
 
(define (install-complex-package)
  (define (make-from-real-imag x y)
    (( get 'make-from-real-imag ' rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
 
  ;;internal procedures
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
 
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
 
  'done)
 
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
 
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
 
 
(define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))
 
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))
 
(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
 
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))
 
(put-coercion 'scheme-number 'complex scheme-number->complex)
 
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2
                        (apply-generic op (t1->t2 a1) a2))
                      (t2->t1
                        (apply-generic op a1 (t2->t1 a2)))
                      (else
                        (error "No method for these types"
                               (list op type-tags))))))
            (error "No method for these types"
                   (list op type-tags)))))))
 
(define (variable? x) (symbol? x))
 
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (=zero?-poly p)
    (=zero?-terms (term-list p)))
  (define (=zero?-terms L)
    (or (empty-termlist? L)
        (and (=zero? (cadr (first-term L))) (=zero?-terms (rest-terms L)))))
 
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
 
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (nega-poly p2))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))
 
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))
 
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((answer ((div-terms (term-list p1)
                                 (term-list p2)))))
          (make-poly (variable p1) (car answer)))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))
 
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
 
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
;  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
 
;  (define (add-terms L1 L2)
;    (cond ((empty-termlist? L1) L2)
;          ((empty-termlist? L2) L1)
;          (else
;            (let ((t1 (first-term L1)) (t2 (first-term L2)))
;              (cond ((> (order t1) (order t2))
;                     (adjoin-term
;                       t1 (add-terms (rest-terms L1) L2)))
;                    ((< (order t1) (order t2))
;                     (adjoin-term
;                       t2 (add-terms L1 (rest-terms L2))))
;                    (else
;                      (adjoin-term
;                        (make-term (order t1)
;                                   (add (coeff t1) (coeff t2)))
;                        (add-terms (rest-terms L1)
;                                   (rest-terms L2)))))))))
  (define (add-terms L1 L2)
    (map + L1 L2))
 
 
;  (define (mul-terms L1 L2)
;    (if (empty-termlist? L1)
;      (the-empty-termlist)
;      (add-terms (mul-term-by-all-terms (first-term L1) L2)
;                 (mul-terms (rest-terms L1) L2))))

  (define (mul-terms L1 L2)
    (fold-right add-terms 
		(the-empty-termlist)
		(stirs 
		  (map (lambda (l2) 
		      (map (lambda (l1)
			     (* l1 l2 )) 
			   L1)) 
		       L2))))

  (define (stirs arglist)
    (define (stirs-local arglist i)
      (cons (padding i (car arglist))
	    (stirs-local (cdr arglist) (+ i 1)))))

  (define (padding i arg)
    (define (make-padding i current)
      (if (zero? i)
	current
	(cons 0 (make-padding (- i 1) current))))
    (append (make-padding 1 '()) arg))




 
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term (+ (order t1) (order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
 
 
;  (define (nega-poly p)
;    (define nega-term-list (map nega-term (term-list p)))
;    (cons (variable p) nega-term-list))
 
  (define (nega-poly p)
    (define nega-term-list (map (lambda (x)(* x -1)) (term-list p)))
    (cons (variable p) nega-term-list))

;  (define (nega-term term)
;    (make-term (order term)
;               (nega (coeff term))))


  (define (tag p) (attach-tag 'polynomial p))
 
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero?-poly p)))
  (put 'nega '(polynomial)
       (lambda (p) (tag (nega-poly p))))
  'done)
 
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
 
(define (install-=zero?-package)
  (define (=zero?-scheme-number n)
    (eq? n 0))
  (define (=zero?-rational x)
    (eq? (car x) 0))
  (define (=zero?-complex x)
    (eq? (magnitude x) 0))
 
  (put '=zero? '(scheme-number)
       (lambda (x) (=zero?-scheme-number x)))
  (put '=zero? '(rational)
       (lambda (x) (=zero?-rational x)))
  (put '=zero? '(complex)
       (lambda (x) (=zero?-complex x)))
  'done)
 
(define (=zero? x) (apply-generic '=zero? x))
 
(define (install-nega-package)
  (define (nega-scheme-number n)
    (- n))
  (put 'nega '(scheme-number)
       (lambda (x) (nega-scheme-number x)))
  'done)
(define (nega x) (apply-generic 'nega x))
 
(install-polynomial-package)
(install-scheme-number-package)
(install-=zero?-package)
 
(define p1 (make-polynomial 'x '(5  -1)))
(define p2 (make-polynomial 'x '(1 -1)))
;(define q1 (make-polynomial 'y `((1 ,p1) (0 ,p2))))
;(define q2 (make-polynomial 'y `((1 ,p2) (0 ,p1))))
 
(print (nega p1))
(print (sub p1 p2))
(print (mul p1 p2))
;(print (sub q1 q2))

