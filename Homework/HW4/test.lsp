(load "hw4.lsp")

(defun checkclause (c r)
(cond 
((null c) nil)
((contain (first c) r) t)
(t (checkclause (rest c) r))
)
)

(defun test-result (res clauses)
(cond
((null res) nil)
((null clauses) t)
((not (checkclause (first clauses) res)) (first clauses))
(t (test-result res (rest clauses)))
)
)

(defun test-file (filename)
(test-result (solve-cnf filename) (second (parse-cnf filename)))
)

(defun contain (x l)
(cond
((null l ) nil)
((= x (first l)) t)
(t (contain x (rest l)))
)
)

