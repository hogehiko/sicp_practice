;グローバル変数を使う版
(define v 100)
(define (f x)
  (set! v (min v x))
   v)

;オブジェクト内変数を使う版
(define (make-f)
  (define v 100)
  (define (f x)
    (set! v (min v x))
    v)
  f)

(define f (make-f))
(print (+ (f 0) (f 1)))
(define f (make-f))
(print (+ (f 1) (f 0)))
