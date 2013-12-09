
;��3-7�������O�ɁA��3-3�̉�

;���ׂĂ̈����ɑ΂��G���[�������Ԃ��֐��@���ƂŎg���܂�
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
    ;password-error�֐��ւ̃}�b�s���O��ǉ�
    (cond ((not (eq? secret-password password)) password-error)
	  ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m))))
  dispatch)

;(define acc (make-account-3-3 100 'secret-password))
;(print ((acc 'secret-password 'withdraw) 40))
;(print ((acc 'some-other-password 'deposit) 50))

;��3.3�̉񓚂Ƀp�X���[�hn�����ۂ݂̂��`�F�b�N���鏈����ǉ���������
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
    ;�V���{��correct-password?�ւ̃}�b�s���O��ǉ�
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
;peter-acc�̃p�X���[�h���������Ȃ�
(print (make-joint peter-acc 'hoge 'rosebud))
(define paul-acc (make-joint peter-acc 'open-sesami 'rosebud))
(print ((paul-acc 'rosebud 'withdraw) 40))
(print ((paul-acc 'open-sesami 'deposit) 50))


