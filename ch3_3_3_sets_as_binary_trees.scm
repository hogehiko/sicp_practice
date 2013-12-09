;��i�؂ɂ��Z�b�g�̕\��

;selectors of binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))
;�m�[�h��(���o���@���̖؁@�E�̖�) �Ƃ��������ɂȂ��Ă���

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
;2.63
;�ȉ��̂Q�̎葱���͂��ꂼ��Q�i�؂����X�g�ɕϊ�����

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;a. �Q�̎葱���͂��ׂĂ̖؂ɑ΂��ē������ʂ𐶂��邩�B�����łȂ���΁A���ʂ͂ǂ��Ⴄ���B�Q�̎葱���͐} 2.16�̂悤�Ȗ؂���ǂ��������X�g�𐶂��邩?

;A.���ʂɈႢ�͂Ȃ��B�ǂ����tree->list������������؂ɑΉ����Ă���B


;trees in figure 2.16
(define (ub-tree entry right-tree)
  (make-tree entry '() right-tree))

(define unbalanced-tree 
  (ub-tree 1 (ub-tree 2 (ub-tree 3 (ub-tree 4 (ub-tree 5 (ub-tree 6 (ub-tree 7 '()))))))))
;(print unbalanced-tree)

;(print (tree->list-1 unbalanced-tree))
;(print (tree->list-2 unbalanced-tree))

;b. n���̗v�f�̒ނ荇���Ă���؂����X�g�ɕϊ�����̂ɕK�v�ȃX�e�b�v���̑����̕p�x�́A2�̎葱���ňႤ���B���Ȃ��Ȃ�A�ǂ��炪���x���������邩�B
;
;A.tree->list-1��append���g�p���Ă���̂ŁA���X�g�����̒T�����J��Ԃ��s����B�����tree->list-2�̂ق����A�v�f���̑����ɑ΂���X�e�b�v���̑����͒x���B

;�ƁA�v�������̌㎎���Ă݂܂��傤�B

;2.64
;�����Â���ꂽ���X�g��ނ荇���Ă���j�i��
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;���X�g�̍ŏ���n���܂ށA�ނ荇���Ă���؂�Ԃ��B
;���ʂ́Acons�ō�����΂ŁA����car�͍\�����ꂽ�؁Acdr�͋C�Ɋ܂܂�Ȃ������v�f�̃��X�g
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) 
	;left-size : n-1��2�̏�
        (let ((left-result (partial-tree elts left-size)))
	  ;left-result:���X�g�̍ŏ���left-size���܂ށA�ނ荇���Ă����
          (let ((left-tree (car left-result))
		;left-tree:�擪����left-size�̗v�f�ō\�����ꂽ��
                (non-left-elts (cdr left-result))
		;non-left-elts:left-tree�Ɋ܂܂�Ȃ������v�f�̃��X�g
                (right-size (- n (+ left-size 1))))
	    	;right-size: n����left-tree�̃T�C�Y�{�P���������l
            (let ((this-entry (car non-left-elts))
		  ;this-entry:left-tree�Ɋ܂܂�Ȃ��������X�g�̂����A
		  ;�擪�̗v�f
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
	          ;right-result left-tree�Ɋ܂܂�Ȃ��������X�g�̂����A
		  ;��Ԗڈȍ~�̗v�f�Aright-size
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
		    ;remaining-elts left-tree, this-entry, right-tree������ɂ��܂܂�Ȃ������v�f�̃��X�g
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;a. partial-tree���ǂ��������A�ł��邾�������Ȑ����������B
;list->tree�����X�g(1 3 5 7 9 11)�ɍ��؂������B

;A.n�̗v�f�̂����A�����̈ʒu(n-1��2�̏��j�ɓ�����v�f�����o���Ƃ��A���o�����O�̕������X�g��partial-tree�ɂ���Ēނ荇���Ă����i�؂ɕϊ��������̂��������؁A���o������̕������X�g��ނ荇���Ă����i�؂ɕϊ��������̂��E�����؂Ƃ����؂��\������B�\�������؂�n+1�ڈȍ~�̗v�f�̃y�A��Ԃ��B

;(print (list->tree (list 1 3 5 7 9 11) ))
(define (inner-range n)
  (if (= n 0)
    '()
    (cons n (inner-range (- n 1)))))
(define (range n) (reverse (inner-range n)))

;(print (range 10))
;(list->tree (range 30000000))
;(print 'done)


;b. list->tree��n�̗v�f�̃��X�g��ϊ�����̂ɕK�v��X�e�b�v���̑����͂ǂ̂��炢���B

;A. ���X�g�̗v�f����Â��o���A�����؂����B�v�f1�ɂ��A�֐��Ăяo�����B�X�e�b�v���̑����̓�(n)�ł���B
;
;�c�Ǝv���̂ł����ǂ��v���܂��H

;2.65
;2.63��2.64�̌��ʂ��g���A�ނ荇������i�؂Ƃ��Ď�������Ă���W����union-set��intersectoin-set����(n)�Ŏ�������B

;��(n)�̌v�Z�����񐔌J��Ԃ��Ă���(n)
;tree->list�Alist->tree�A����я����t����ꂽ���X�g�Ƃ��Ă̏W���Ŏg����
;������g�ݍ��킹�Ď������悤�A�Ƃ����̂���{�I�ȕ��j�B

;�����t����ꂽ���X�g�Ƃ��Ă̏W���ɂ�����intersectoin-set�̓�(n)
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))
;2.62�Ŏ������������t����ꂽ���X�g�Ƃ��Ă̏W���ɂ�����union-set����(n)
(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (union-set (cdr set1) set2))
		      ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      ((< x2 x1)
		       (cons x2 (union-set set1 (cdr set2)))))))))

;(print (union-set (list 1 2 5 6) (list 4 5 7)))

;�v�Z�ʂ���(n)��tree->list2���g�p����
(define tree->list tree->list-1)
(define (intersection-tree-set set1 set2)
  (list->tree (intersection-set (tree->list set1)
			    (tree->list set2))))
(define (union-tree-set set1 set2)
  (list->tree (union-set (tree->list set1)
			 (tree->list set2))))
;(define r (range 1000000))
;(define tree1 (list->tree (range 4000000)))
;(define tree2 (list->tree (range 4000000)))
;(print (tree->list tree1))
;(print (tree->list tree2))
;(time (intersection-tree-set tree1 tree2))
;(union-tree-set tree1 tree2)

;2.66
;record�̓L�[�ƒl������
(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))

(define tree (make-tree (make-record 5 'e)
			(make-tree (make-record 1 'a) '()'())
			(make-tree (make-record 10 'j) 
				   (make-tree (make-record 7 'g) '() '())
				   '())))
(print (lookup 7 tree))
