; TREE-CONTAINS, which takes two arguments N and TREE, and checks whether number N appears in the ordered tree TREE.
(defun TREE-CONTAINS(N TREE)
	(cond 
		((null TREE) NIL)
		((atom TREE) (= N TREE)) ;tree only contains one element 
		((= N (second TREE)) t)
		((< N (second TREE)) (TREE-CONTAINS N (first TREE))) 
		((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
	)
)

;TREE-MIN, which takes one argument TREE, and returns the minimum number appearing in the ordered tree TREE.
(defun TREE-MIN (TREE)
	(cond
		((null TREE) NIL)
		((atom TREE) TREE)
		(t (TREE-MIN (first TREE)))
	)
)

;TREE-ORDER, which takes one argument TREE, and returns an pre-ordered list of the numbers appearing in the ordered tree TREE.

(defun TREE-ORDER (TREE)
	(cond
		((null TREE) NIL)
		((atom TREE) (list TREE))
		(t 
			(cons 
				(second TREE)				
				(append 
					(TREE-ORDER (first TREE))
					(TREE-ORDER (third TREE))
				)		
			)
		)
	)
)

;SUB-LIST, that takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN.

(defun SUB-LIST (L START LEN)
	(cond
		((null L) NIL)
		((= LEN 0) NIL)
		((= START 0) 
			(cond 
				((= LEN 1) (list (car L)))
				(t (append (list (car L)) (SUB-LIST (cdr L) START (- LEN 1))))
			)
		)
		(t (SUB-LIST (cdr L) (- START 1) LEN))
	)
)

;SPLIT-LIST, that takes a list L, and returns a list of two lists L1 and L2, in that order

(defun SPLIT-LIST (L)
	(let
		((len (length L)))
		(if (evenp len)
			(list 
				(SUB-LIST L 0 (/ len 2))
				(SUB-LIST L (/ len 2) (/ len 2))
			)
			(list 
				(SUB-LIST L 0 (/ (+ len 1) 2))
				(SUB-LIST L (/ (+ len 1) 2) (/ (- len 1) 2))
			)
		)
	)
)


;BTREE-HEIGHT, which takes a binary tree TREE, and returns the height of TREE

(defun BTREE-HEIGHT (TREE)
	(cond 
		((null TREE) 0)
		((atom TREE) 0)
		(t 
			(let 
				(
					(left (BTREE-HEIGHT (first TREE)))
					(right (BTREE-HEIGHT (second TREE)))
				)
				(cond 
					((< left right) (+ 1 right))
					(t (+ 1 left))
				)
			)
		)
	)
)

;LIST2BTREE, that takes a non-empty list of atoms LEAVES, and returns a binary tree

(defun LIST2BTREE (LEAVES)
	(cond 
		((null LEAVES) NIL)
		(t 
			(let 
				((len (length LEAVES)))
				(cond 
					((= len 1) LEAVES)
					((= len 2) LEAVES)
					((= len 3) (list (list (car LEAVES) (cadr LEAVES)) (caddr LEAVES)))
					(t 
						(list 
							(LIST2BTREE (first (SPLIT-LIST LEAVES))) 
							(LIST2BTREE (second (SPLIT-LIST LEAVES)))
						)
					)
				)				
			)
		)
	)
)

;BTREE2LIST, that takes a binary tree TREE as input, and returns a list of atoms

(defun BTREE2LIST(TREE)
	(cond 
		((null TREE) NIL)
		((atom TREE) (list TREE))
		(t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))
	)
)


;IS-SAME, that takes two LISP expressions E1 and E2 whose atoms are all numbers, and checks whether the expressions are identical. 

(defun IS-SAME (E1 E2)
	(cond 
		((and (null E1) (null E2)) t)
		((and (atom E1) (atom E2)) (= E1 E2))
		((and (listp E1) (listp E2)) 
			(and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))
		)
		(t NIL)
	)
)
