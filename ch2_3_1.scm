;(list 'a 'b 'c)
;
;(a b c)
(print (list 'a 'b 'c))

;(list (list 'george))
;
;((george))
(print (list (list 'george)))

;(cdr '((x1 x2)(y1 y2)))
;(cdr (list '(x1 x2) '(y1 y2)))
;(list '(y1 y2))
;(list (list 'y1 'y2))
;
;((y1 y2))
(print (cdr '((x1 x2)(y1 y2))))

;(car (cdr '((x1 x2) (y1 y2))))
;(car (cdr (list '(x1 x2) '(y1 y2))))
;(car '((y1 y2)))
;(car (list '(y1 y2)))
;'(y1 y2)
;(list 'y1 'y2)
;
;(y1 y2)
(print (cadr '((x1 x2) (y1 y2))))

;(pair? (car '(a shor list)))
;(pair? (car (list 'a 'shor 'list)))
;(pair? 'a)
;#f
(print (pair? (car '(a shor list))))

;(memq 'red '((red shoes) (blue socks)))
;(memq 'red (list '(red shoes) '(blue socks)))
;#f
(print (memq 'red '((red shoes) (blue socks))))

;(memq 'red '(red shoes blue socks))
;(memq 'red (list 'red 'shoes 'blue 'socks))
;(list 'red 'shoes 'blue 'socks)
;
;(red shoes blue socks)
(print (memq 'red '(red shoes blue socks)))

;equal
(define (equal? one another)
  (if (pair? one)
    (and (equal? (car one) (car another))
	 (equal? (cdr one) (cdr another)))
    (eq? one another)))

(print (equal? '(this is a list)'(this is a list)))
(print (equal? '(this is a list)'(this i(is a) list)))

(print (car ''abracadabra))
(print (car '(quote abracadabra)))
(print (car (list 'quote 'abracadabra)))

(print (list 'car (list 'quote '(a b c))))

