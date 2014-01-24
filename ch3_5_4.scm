#lang planet neil/sicp

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (take n s)
  (if (> n 1)
      (cons (stream-car s) (take (- n 1) (stream-cdr s)))
      '()))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1 ))))

;(define (stream-map proc s)
;  (if(stream-null? s)
;     the-empty-stream
;     (cons-stream ( proc (stream-car s))
;                  (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      `done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (scale-stream stream factor)
  (stream-map (lambda (x ) (* x factor)) stream ))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (add-stream s1 s2)
  (stream-map + s1 s2))


;(define (integral integrand initial-value dt)
;  (define int
;    (cons-stream initial-value
;                 (add-stream (scale-stream integrand dt)
;                             int)))
;  int)

;(define (integral delayed-integrand initial-value dt)
;  (define int 
;    (cons-stream initial-value
;                 (let ((integrand (force delayed-integrand)))
;                   (add-stream (scale-stream integrand dt)
;                               int ))))
;  int)

;q.3.77 
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand  (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay ( stream-cdr integrand))
                               (+ (* dt ( stream-car integrand ))
                                  initial-value)
                               dt)))))

;solve 最初のdyを評価できない
;(define (solve f y0 dt)
;  (define y (integral dy y0 ))
;  (define dy (stream-map f y))
;  y)
;(stream-ref (stream-enumerate-interval 0 10) 4)
;(stream-ref (integral (delay (stream-enumerate-interval 0 100)) 0 0.001) 4)

;(define (solve f y0 dt)
  
;  (define y (integral (delay dy) y0 dt))
;  (define dy (stream-map f y))
;  y)


;試す
(define (solve f y0 dt)
  (define dy '() )
  (define y (integral (delay dy) y0 dt))
  (set! dy (stream-map f y))
  y)
;(define (solve2 f y0 dt)
;  (let* ((y (integral (delay dy) y0 dt))
;         (dy (stream-map f y)))
;    y))


;(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

;q.3.78
;本当はこうしたい
(define (solve-2nd-does-not-work a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-stream (scale-stream y a)
                          (scale-stream dy b)))
  y)
;が、動かない。
(define (solve-2nd a b dt y0 dy0)
  (define dy '())
  (define ddy '())
  (define y (integral (delay dy) y0 dt))
  (set! dy (integral (delay ddy) dy0 dt))
  (set! ddy (add-stream (scale-stream dy a)
                        (scale-stream y b)))
  y)

;q.3.79
(define (solve-2nd-2 f dt y0 dy0)
  (define dy '())
  (define ddy '())
  (define y (integral (delay dy) y0 dt))
  (set! dy (integral (delay ddy) dy0 dt))
  (set! ddy (stream-map f dy y))
  y)

(define a 1)
(define b 2)
(define y0 10)
(define dy0 20)
;(stream-ref (solve-2nd a b 0.001 y0 dy0) 1000)
;(stream-ref 
; (solve-2nd-2 
;  (lambda (dy y)(+ (* a dy) (* b y))) 
;  0.001 
;  y0 
;  dy0) 
; 1000)

;q.3.80
(define (RLC R L C dt)
  (define (init vc0 iL0)
    (define dvC '())
    (define diL '())
    
    (define vC (integral (delay dvC) vc0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (set! dvC (scale-stream iL (/ -1 C)))
    (set! diL (add-stream (scale-stream vC (/ 1 L))
                          (scale-stream iL (* -1 (/ R L)))))
    (cons vC iL))
  init)

(define stream-pair ((RLC 1 1 0.2 0.1) 10 0) )
(define vC (car stream-pair))
(define iL (cdr stream-pair))
;(stream-ref vC 1000)
;(stream-ref iL 1000)


(take 10 vC)
(take 10 iL)
  
         
         
(define rand-a 283829198319)
(define rand-b 123412341234)
(define rand-m 0987089709870987)

(define (rand-update x)
  (remainder (+ (* rand-a x) rand-b) rand-m))


(define random-init 1)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
      
(define x random-init)
(define (generate)
  (set! x (rand-update x))
  x)
(define (reset initial-value)
  (set! x initial-value))
(define (dispatch m)
  (cond (((eq? m 'generate) generate)     
         ((eq? m 'reset) reset))))
  
          



