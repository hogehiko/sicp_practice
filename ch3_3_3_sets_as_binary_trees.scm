;二進木によるセットの表現

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
;ノードは(見出し　左の木　右の木) という実装になっている

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
;以下の２つの手続きはそれぞれ２進木をリストに変換する

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

;a. ２つの手続きはすべての木に対して同じ結果を生じるか。そうでなければ、結果はどう違うか。２つの手続きは図 2.16のような木からどういうリストを生じるか?

;A.結果に違いはない。どちらのtree->list実装もあらゆる木に対応している。


;trees in figure 2.16
(define (ub-tree entry right-tree)
  (make-tree entry '() right-tree))

(define unbalanced-tree 
  (ub-tree 1 (ub-tree 2 (ub-tree 3 (ub-tree 4 (ub-tree 5 (ub-tree 6 (ub-tree 7 '()))))))))
;(print unbalanced-tree)

;(print (tree->list-1 unbalanced-tree))
;(print (tree->list-2 unbalanced-tree))

;b. nこの要素の釣り合っている木をリストに変換するのに必要なステップ数の増加の頻度は、2つの手続きで違うか。おなじなら、どちらがより遅く増加するか。
;
;A.tree->list-1はappendを使用しているので、リスト末尾の探索が繰り返し行われる。よってtree->list-2のほうが、要素数の増加に対するステップ数の増加は遅い。

;と、思うがこの後試してみましょう。

;2.64
;順序づけられたリストを釣り合っているニ進木
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

;リストの最初のn個を含む、釣り合っている木を返す。
;結果は、consで作った対で、そのcarは構成された木、cdrは気に含まれなかった要素のリスト
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2))) 
	;left-size : n-1と2の商
        (let ((left-result (partial-tree elts left-size)))
	  ;left-result:リストの最初のleft-size個を含む、釣り合っている木
          (let ((left-tree (car left-result))
		;left-tree:先頭からleft-sizeの要素で構成された木
                (non-left-elts (cdr left-result))
		;non-left-elts:left-treeに含まれなかった要素のリスト
                (right-size (- n (+ left-size 1))))
	    	;right-size: nからleft-treeのサイズ＋１を引いた値
            (let ((this-entry (car non-left-elts))
		  ;this-entry:left-treeに含まれなかったリストのうち、
		  ;先頭の要素
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
	          ;right-result left-treeに含まれなかったリストのうち、
		  ;二番目以降の要素、right-size
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
		    ;remaining-elts left-tree, this-entry, right-treeいずれにも含まれなかった要素のリスト
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;a. partial-treeがどう動くか、できるだけ明快な説明をかけ。
;list->treeがリスト(1 3 5 7 9 11)に作る木をかけ。

;A.n個の要素のうち、中央の位置(n-1と2の商）に当たる要素を見出しとし、見出しより前の部分リストをpartial-treeによって釣り合っている二進木に変換したものを左部分木、見出しより後の部分リストを釣り合っている二進木に変換したものを右部分木とした木を構成する。構成した木とn+1個目以降の要素のペアを返す。

;(print (list->tree (list 1 3 5 7 9 11) ))
(define (inner-range n)
  (if (= n 0)
    '()
    (cons n (inner-range (- n 1)))))
(define (range n) (reverse (inner-range n)))

;(print (range 10))
;(list->tree (range 30000000))
;(print 'done)


;b. list->treeがn個の要素のリストを変換するのに必要んステップ数の増加はどのくらいか。

;A. リストの要素を一つづつ取り出し、部分木を作る。要素1つにつき、関数呼び出し一回。ステップ数の増加はΘ(n)である。
;
;…と思うのですがどう思います？

;2.65
;2.63と2.64の結果を使い、釣り合った二進木として実装されている集合のunion-setとintersectoin-setをΘ(n)で実装せよ。

;Θ(n)の計算を一定回数繰り返してもΘ(n)
;tree->list、list->tree、および順序付けられたリストとしての集合で使った
;実装を組み合わせて実装しよう、というのが基本的な方針。

;順序付けられたリストとしての集合におけるintersectoin-setはΘ(n)
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
;2.62で実装した順序付けられたリストとしての集合におけるunion-setもΘ(n)
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

;計算量がΘ(n)のtree->list2を使用する
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
;recordはキーと値を持つ
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
