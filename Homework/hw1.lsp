(defun TREE-CONTAINS(N TREE)
	(cond 
		((null TREE) NIL)
		((atom TREE) (= N TREE))
		((< N (cadr TREE)) (TREE-CONTAINS N (car TREE)))
		((> N (cadr TREE)) (TREE-CONTAINS N (cddr TREE)))
		(t (= N (cadr TREE)))
	)
)