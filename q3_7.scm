
;問3-7を解く前に、問3-3の回答

;すべての引数に対しエラー文字列を返す関数　あとで使います
(define (password-error . x) "Incorrect password")

(define (make-account-3-3 balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch password m)
    ;password-error関数へのマッピングを追加
    (cond ((not (eq? secret-password password)) password-error)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)

;(define acc (make-account-3-3 100 'secret-password))
;(print ((acc 'secret-password 'withdraw) 40))
;(print ((acc 'some-other-password 'deposit) 50))

;問3.3の回答にパスワードnお正否のみをチェックする処理を追加したもの
(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    ;シンボルcorrect-password?へのマッピングを追加
    (cond ((eq? m 'correct-password?) (eq? password secret-password))
          ((not (eq? secret-password password)) password-error)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)


(define (make-joint acc original-password new-password)
  (define (dispatch password m)
    (cond ((eq? m 'correct-password?) (eq? password new-password))
	  ((not (eq? new-password password)) password-error) 
	  (else (acc original-password m))))
  (if (acc original-password 'correct-password?)
    dispatch
    "Incorrect original password"))

(define peter-acc (make-account 100 'open-sesami))
;peter-accのパスワードが正しくない
(print (make-joint peter-acc 'hoge 'rosebud))
(define paul-acc (make-joint peter-acc 'open-sesami 'rosebud))
(print ((paul-acc 'rosebud 'withdraw) 40))
(print ((paul-acc 'open-sesami 'deposit) 50))


