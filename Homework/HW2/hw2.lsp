;Depth-frist search of a tree
;Variables: TREE (list of a tree)
;The function check where the list is null, if it is null return NIL
;Check if it's passed an atom, return a list of that atom if it is true 
;Check if it's passed a list with length of one, return that list if it is true 
;append the right child of the tree to the head of the left child of the tree
;return a list of tree following the DFS order. 
(defun DFS (TREE)
    (cond 
        ((null TREE) NIL) ;check whether the tree is empty 
        ((atom TREE) (list TREE)) ;if the tree only have one element. just return it as a list
        ((= (length TREE) 1) (DFS (car TREE))) ;if the tree only have a list of length 1, pass the atom to DFS again. 
        (t (append (DFS (cdr TREE)) (DFS (car TREE)))) ;check the left child before the right child, make them a list 
    )
)

;Depth-first iterative-deepening helper function, which only traverse up to certain depth 
;DFID_single takes two arguments TREE and DEPTH
;return a list of all nodes in the tree up to certain depth following the DFS order. 
(defun DFID_Single (TREE DEPTH)
    (cond 
        ((< DEPTH 0) NIL) ;If the depth is invalid, return NIL
        ((null TREE) NIL) ;If the Tree is invalid, return NIL
        ((atom TREE) (list TREE)) ; If the Tree contains only one atom, return it as a list 
        ((= (length TREE) 1) (DFID_Single (car TREE) (- DEPTH 1)));if the lenght of the tree is 1, then convert it to the atom case. Minus 1 from the depth can let the recursion stop after this step. 
        (t (append (DFID_Single (cdr TREE) DEPTH) (DFID_single (car TREE) (- DEPTH 1)))) ;Process the right most child before the rest childs, and append them to a list and return it. 
    )
)

;Depth-first iterative-deepening, return a list of all elements in every depth 
;It takes two variables, tree and depth  
(defun DFID (TREE DEPTH)
    (cond 
        ((< DEPTH 0) NIL) ;If the Depth is invalid, return NIL
        ((null TREE) NIL) ;If the Tree is invalid, return NIL
        (t (append (DFID TREE (- DEPTH 1)) (DFID_Single TREE DEPTH))) ;Append all the list returned by DFID_Single to a list and return it.
    )
)

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond
    ((and (equal (length s) 3) (equal (first s) 3) (equal (second s) 3) (not (third s))) t) ;check the length and every single element are correct. 
    (t NIL)
  )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (cond
    ((null s) NIL) ;invalid list 
    ((> (+ m c) 2) NIL) ;more people than the boat can carry 
    ((> m (first s)) NIL) ;move more missinoaries than there are 
    ((> c (second s)) NIL) ;move more cannibals than there are
    ((and (> (- (first s) m) 0) (> (- (second s) c) (- (first s) m))) NIL) ;resulting more C than M on the current side.
    ((and (> (+ (- 3 (first s)) m) 0) (> (+ (- 3 (second s)) c) (+ (- 3 (first s)) m))) NIL) ;resulting more C than M on the other side.
    (t (list (list (+ (- 3 (first s)) m) (+ (- 3 (second s)) c) (not (third s)))))
  )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 0 2) (next-state s 2 0)) ; append all possbile outcome together 
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond
    ((= (length states) 0) NIL) ;base case, if the length of state is zero return NIL (not found)
    ((equal s (car states)) t) ;check if the top matches s.
    (t (on-path s (cdr states))) ;recursion on the rest 
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
  (cond 
    ((= (length states) 0) NIL) ;if the states is empty, return NIL
    ((mc-dfs (first states) path) (mc-dfs (first states) path)) ;if there is a path, return that path. 
    (t (mult-dfs (rest states) path)) ;otherwise, try another state
  )
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond 
    ((final-state s) (append path (list s))) ;if it reaches the final state, append path to the initial state  
    ((on-path s path) nil) ;revisit a node already on the search path
    (t (mult-dfs (succ-fn s) (append path (list s)))) ;do dfs on all successors
  ) 
)



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))



