(load "parse_cnf.lsp")

; Sat function: checks whether delta is satisfied.
; it takes in two variables n and delta
; n: number of variables
; delta: propositional sentence in CNF 
(defun sat? (n delta)
    (let*
        ((variable_list (update_vlist_freq (initialize_nlist n) delta))
        (curr_shortest_clause_list (find_shortest_clause delta))
        (curr_mfreq_var (find_most_freq_var variable_list (second curr_shortest_clause_list))))
        (print variable_list)
        (print curr_shortest_clause_list)
        (print curr_mfreq_var)
        
        ;(get_var (replace_var variable_list curr_mfreq_var T) curr_mfreq_var)
        ;(check_delta delta (replace_var variable_list curr_mfreq_var T))
        ;(check_invalid_delta (check_delta delta (replace_var variable_list curr_mfreq_var T)))
        (sat_helper n delta variable_list 0 '() '() delta T)
    )
)

(defun sat_helper (n updated_delta updated_variable_list update_count bt_vlist bt_dlist delta next-state)
    ;(print update_count)
    ;(print updated_variable_list)
    ;(print bt_vlist)
    ;(print bt_dlist)
    (if (= update_count (length updated_variable_list)) ; if all variables have been assigned 
        (if (not (check_invalid_delta (check_delta updated_delta updated_variable_list))) ;first if then
            (convert_result updated_variable_list) ; Then -> OUTPUT the final RESULT 
            (print "BT 1");(NIL) ; Else ;BACKTRACK 
        ) ; second if 
        (let*
            ((new_variable_list updated_variable_list)
            (curr_shortest_clause_list (find_shortest_clause updated_delta))
            (curr_mfreq_var (find_most_freq_var new_variable_list (second curr_shortest_clause_list))))
            (if (not (check_invalid_delta (check_delta updated_delta (replace_var new_variable_list curr_mfreq_var next-state))))
                (sat_helper n (check_delta updated_delta (replace_var new_variable_list curr_mfreq_var next-state)) 
                                (replace_var new_variable_list curr_mfreq_var next-state) 
                                (+ update_count 1)
                                (cons curr_mfreq_var bt_vlist)
                                (cons (+ (third curr_shortest_clause_list) 1) bt_dlist)
                                delta
                                next-state
                                ) ;then
                (if (not (check_invalid_delta (check_delta updated_delta (replace_var new_variable_list curr_mfreq_var (not next-state)))))
                    (sat_helper n (check_delta updated_delta (replace_var new_variable_list curr_mfreq_var (not next-state))) 
                                (replace_var new_variable_list curr_mfreq_var (not next-state)) 
                                (+ update_count 1)
                                (cons curr_mfreq_var bt_vlist)
                                (cons (+ (third curr_shortest_clause_list) 1) bt_dlist)
                                delta
                                next-state
                                )
                    ; backtrack
                    (sat_helper n (replace_delta updated_delta (first bt_dlist) (first bt_dlist) delta) 
                                    (replace_var updated_variable_list (abs_value (first bt_vlist)) 3)
                                    update_count
                                    (rest bt_vlist) 
                                    (rest bt_dlist)
                                    delta
                                    (not next-state)
                    )            
                )
            )
        )
    ) ; first if
)



; intialize a list with size n to 0
(defun initialize_nlist (n)
    (cond
        ((< n 0) (print "initialize_nlist 'n' is smaller than 0") NIL)
        ((= n 0) '())
        (t (cons 0 (initialize_nlist (- n 1))))
    )
)

; return the absolute value of the input elemnt 
(defun abs_value (value)
    (cond
        ((< value 0) (* value -1))
        (t value)
    )
)

; update list's value to the number of apperance in delta
; variable_list must be initialized already 
; return the newest variable_list 
(defun update_vlist_freq (variable_list delta)
    (cond
        ((null delta) variable_list)
        (t (update_vlist_freq (update_clause_freq (first delta) variable_list) (rest delta))) 
    )
)

; get the num's variable from variables
; num: a int, index of the variable (STARTING FROM 1)
; variables: list of all variables 
(defun get_var (variable_list num)
    (cond
        ((null variable_list) (print "get_var 'variable_list' is empty") NIL)
        ((< num 1) (print "get_var 'num' is invalid") NIL)
        ((= num 1) (first variable_list))
        (t (get_var (rest variable_list) ( - num 1)))
    )
)

; find the most frequent value in the given clause
; given a list (a clause) return the index
(defun find_most_freq_var (variable_list clause)
    (cond 
        ((null variable_list) (print "find_most_freq_var 'variable_list' is empty")NIL)
        (t (find_most_freq_var_core variable_list clause 0 0))
    )
)

; replace the variable list with the right updated value. 
; it takes in three variables
; num: a int, index of the variable (STARTING FROM 1)
; variables: list of all variables 
; updated_value: new value of that index 
(defun replace_var (variable_list num updated_value)
    (cond
        ((null variable_list) (print "replace_var 'variable_list' is empty") NIL)
        ((> num (length variable_list)) (print "replace_var 'num' is larger then the list's length") NIL)
        ((< num 1) (print "replace_var 'num' is negative") NIL)
        ((= num 1) (cons updated_value (rest variable_list)))
        (t (cons (first variable_list) (replace_var  (rest variable_list) ( - num 1) updated_value)))
    )

)

(defun replace_delta (new_delta num get_num old_delta)
    (cond
        ((null new_delta) (print "replace_delta 'new_delta' is empty") NIL)
        ((> num (length new_delta)) (print "replace_delta 'num' is larger then the list's length") NIL)
        ((< num 1) (print "replace_delta 'num' is negative") NIL)
        ((= num 1) (cons (get_var old_delta get_num) (rest new_delta)))
        (t (cons (first new_delta) (replace_delta  (rest new_delta) ( - num 1) get_num old_delta)))
    )   
)

; function for finding the __FIRST__ shortest clause
; it takes one variable delta (a list of clauses)
; it returns a list (length of clause, clause, clause_index)
(defun find_shortest_clause (delta)
    (cond
        ((null delta) (print "find_shortest_clause 'delta' is empty") NIL)
        (t (find_shortest_clause_helper delta 99999 NIL NIL 0))
    )
)

; Check this clause is T or not. 
; If the clause's variables have not been signed yet, return the list
; If it is T, return T.
; Otherwise, return NIL. 
(defun check_clause (clause variable_list)  
    (cond
        ((null clause) NIL)  
        ((equal T clause) T)
        ((atom clause)
            (cond
                ((equal t (get_var variable_list (abs_value clause))) (cond ((< clause 0) NIL) (t t)))
                ((equal NIL (get_var variable_list (abs_value clause)))  (cond ((< clause 0) t) (t NIL)))
                ((numberp (get_var variable_list (abs_value clause))) (list clause))
                (t (print "check_clause second cond failed"))
            )
        )
        (t 
            (cond
                ((equal t (check_clause (first clause) variable_list)) t)
                ((equal NIL (check_clause (first clause) variable_list)) (check_clause (rest clause) variable_list))
                ((listp (check_clause (first clause) variable_list)) 
                    (cond
                        ((null (rest clause)) 0)
                        (t 
                            (let* ((check_result (check_clause (rest clause) variable_list)))
                                (cond
                                    ((equal NIL check_result) 0)
                                    ((equal t check_result) t)
                                    ((listp check_result) 0)
                                    ((= 0 check_result) (check_clause (rest clause) variable_list))
                                )       
                            )                    
                        )
                    )
                )
                (t (print "check_clause third cond failed"))
            )
        )
    )
)

; Check delta is satisifed or not
(defun check_delta (delta variable_list)
    (cond
        ((null delta) NIL)
        (t (let 
            ((clause_result (check_clause (first delta) variable_list))) 
            ;(print clause_result)
            (cons (cond
                ((equal clause_result 0) (first delta))
                (t clause_result)
            ) (check_delta (rest delta) variable_list))
        ))
        ;(t (and (check_clause (first delta) variable_list) (check_delta (rest delta) variable_list)))
    )
)

; return t if the updated_delta contins a NIL
; otherwise return NIL 
(defun check_invalid_delta (updated_delta)
    (cond
        ((null updated_delta) NIL)
        ((equal NIL (first updated_delta)) t)
        (t (check_invalid_delta (rest updated_delta)))
    )
)

; ========================= ONLY for complete variable list ============================

; convert result
; convert from t/NIL list to (-1 2 3 -4 ..)
(defun convert_result (variable_list)
    (convert_result_helper variable_list 0)
)

; ========================= Helper Function ============================
; ========================= for easy recrusion =========================

(defun find_shortest_clause_helper (delta min_length min_clause min_index current_index)
    (cond 
        ((equal T (first delta)) (find_shortest_clause_helper (rest delta) min_length min_clause min_index (+ current_index 1)))
        ((null delta) (list min_length min_clause min_index)) ;when there is nothing left in delta, return the min
        ((< (length (first delta)) min_length) (find_shortest_clause_helper (rest delta) (length (first delta)) (first delta) current_index (+ current_index 1))) ;if new clause length is smaller than the stored clause, replace it.
        (t (find_shortest_clause_helper (rest delta) min_length min_clause min_index (+ current_index 1))) ; otherwise, continue
    )
)

(defun update_clause_freq (clause variable_list)
    (cond
        ((null clause) variable_list)
        (t (update_clause_freq (rest clause) (replace_var variable_list (abs_value (first clause)) (+ 1 (get_var variable_list (abs_value (first clause)))))))
    )
)

(defun find_most_freq_var_core (variable_list clause max_index current_max)
    (cond
        ((null clause) max_index)
        ((equal T (get_var variable_list (abs_value (first clause)))) (find_most_freq_var_core variable_list (rest clause) max_index current_max))
        ((equal NIL (get_var variable_list (abs_value (first clause)))) (find_most_freq_var_core variable_list (rest clause) max_index current_max))
        ((> (get_var variable_list (abs_value (first clause))) current_max) (find_most_freq_var_core variable_list (rest clause) (abs_value (first clause)) (get_var variable_list (abs_value (first clause))) ))
        (t (find_most_freq_var_core variable_list (rest clause) max_index current_max))
    )
)

(defun convert_result_helper (variable_list count)
    (cond
        ((atom variable_list)
            (cond 
                ((equal t variable_list) count)
                (t (* count -1))
            )
        )
        (t (cons (convert_result_helper (first variable_list) (+ count 1)) 
            (cond 
                ((null (rest variable_list)) NIL)
                (t (convert_result_helper (rest variable_list) (+ count 1)))
            ))
        )
    )
)