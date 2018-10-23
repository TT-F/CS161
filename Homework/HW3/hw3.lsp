;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
	(cond
		((null s) t) ;if the s is NIL or empty, we return t. otherwise, the last element "NIL" of the s will return NIL.  
		((atom s) ;base case, if it is an atom, check if it is valid goal-state. 
			(and (not(isBox s)) (not(isKeeper s)))
		)
		(t (and (goal-test (first s)) (goal-test (rest s)))) ;recrusively check the first element of the list. 
	)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 0 y x) (try-move s 1 y x) (try-move s 2 y x) (try-move s 3 y x)))
	 )
    (cleanUpList result);end
   );end let
  );

; Direction Notation 
;	Up: 0
; Down: 1
; Left: 2
; Right: 3

; Coordinate System: 
; Top left corner is (0, 0)

;	try-move fucntion takes a state, a direction, and xy coordinates. 
; It returns a state if it is able to move on that direction,
; otherwise, return NIL. 
(defun try-move (s direction r c)
	(let 
		((next-coordinate (coordinate-direction r c direction))) ;get the new coordination base on the direction 
		(if (not next-coordinate) NIL) ; if the next-coordinate is NIL, return NIL. 
		(valid-move s (first next-coordinate) (second next-coordinate) direction r c)  ; change next/ next-next square 
	)
)

;  c||
;r
;-
;-

; It takes in a State S, a row number r, and a column number c
; It returns the integer content of state S at square (r,c).
(defun get-square (s r c)
	; check if it's out of range, return NIL if true
	(cond
		((or (>= r (length s)) (>= c (length (car s)))) NIL)
		((or (< r 0) (< c 0)) NIL)
		(t (car (nthcdr c (car (nthcdr r s))))); return the element at r c 
	)
)

; replace c's element of the l with v. 
; helper function for set-square
(defun set-column (l c v)
	(cond
		((null l) NIL) ;list is empty 
		((>= c (length l)) NIL) ;column # is longer than the lenght of the list 
		((< c 0) NIL)
		((= c 0) (cons v (nthcdr 1 l))) ; recrusion base case append the element to the rest 
		(t (cons (first l) (set-column (rest l) (- c 1) v))) ; keep iterating through the list until we reach the goal 
	)
)

; It takes in a state S, a row number r, a column number c, and
; a square content v (integer). 
; This function should return a new state S’ that is obtained by setting
; the square (r,c) to value v.
(defun set-square (s r c v)
	; check if it's out of range, return NIL if true
	(cond 
		((null s) NIL) ;state is empty 
		((>= r (length s)) NIL) ;row # is longer than the lenght of the state
		((>= c (length (car s))) NIL) ;column # is longer than the lenght of the list 
		((or (< r 0) (< c 0)) NIL) 
		((= r 0) (cons (set-column (car s) c v) (nthcdr 1 s))) ; recrusion base case append the element to the rest 
		(t (cons (first s) (set-square (rest s) (- r 1) c v))) ; keep iterating through the list until we reach the goal 
	)
)

; helper function of valid-move 
; doesn't include next squre is a box 
(defun valid-basic-move (curr-element)
	(cond 
		; return NIL if next move is invalid 
		((not curr-element) NIL)
		; check if it's a wall, return NIL if true
		((isWall curr-element) NIL)
		; check if it's BLANK/GOAL, return t if true
		((or (isBlank curr-element ) (isStar curr-element)) t)
		(t NIL)
	)
)

; change coordinate base on direction
; you must check if the new coordinate is valid by yourself
; it returns a list '(r c)
(defun coordinate-direction (r c direction)
	(cond 
		((= direction 0) (list (- r 1) c))
		((= direction 1) (list (+ r 1) c))
		((= direction 2) (list r (- c 1)))
		((= direction 3) (list r (+ c 1)))
		(t NIL)
	)
)

; helper function for setting the first square to its original statu 
; keeper+ goal -> goal 
; Keeper  -> blank
(defun keeper_set_back (s r c)
	(let 
		((keeper-element (get-square s r c)))
		(cond 
			((isKeeper keeper-element) (set-square s r c blank))
			((isKeeperStar keeper-element) (set-square s r c star))
			(t NIL) ;this should not be executed 
		)
	)
)

; helper function for setting the second square to its new statu
; blank -> keeper
; star -> keeperstar
(defun set_to_keeper (s r c old-r old-c)
	(let 
		((curr-element (get-square s r c)))
		(cond ;if it is a valid move 
			;if next is blank (Box is only valid when valid-move is in box section) USE Inheritance maybe?
			((or (isBlank curr-element) (isBox curr-element)) (set-square (keeper_set_back s old-r old-c) r c keeper))
			;if next is goal/star (Boxstar is only valid when valid-move is in box section) USE Inheritance maybe?
			((or (isStar curr-element) (isBoxStar curr-element)) (set-square (keeper_set_back s old-r old-c) r c keeperstar))
			(t NIL) ;this step should not be executed 
		)
	)
)

; this functin check if location at r c is a valid space for next state. 
; ATTENTION: r c should be the value of the next coordinate 
; direction is to help identify the next move direction for the box scenrio 
(defun valid-move (s r c direction old-r old-c)
	(let 
		((curr-element (get-square s r c)))
		(cond 
			((not curr-element) NIL) ; out of range, invalid move.
			((valid-basic-move curr-element)  (set_to_keeper s r c old-r old-c)) ; setting first two elements to their right status
			; check if it's a BOX/BOX+GOAL, if it is true, check next coordinates in same direction is valid or not.
			; BLANK/GOAL are valid 
			; WALL/BOX/BOX+GOAL are invalid 
			((or (isBox curr-element) (isBoxStar curr-element))
				(let 
					((next-coordinate (coordinate-direction r c direction))) ; get the coordinate of the square next to the box 
					(if (not next-coordinate) NIL) ; check if it returns NIL
					(if (or (< (first next-coordinate) 0) (< (second next-coordinate) 0)) NIL)
					(let
						((next-element (get-square s (first next-coordinate) (second next-coordinate)))) ; get the element of the next square 
						(cond 
							((valid-basic-move next-element) ; check if it's a space you can move to 
								;(print next-element)
								(cond ; if it is 
									; chagne second square 
									; second square is a blank
									((isBlank next-element) (set-square (set_to_keeper s r c old-r old-c) (first next-coordinate) (second next-coordinate) box))
									; second square is a star/goal
									((isStar next-element) (set-square (set_to_keeper s r c old-r old-c) (first next-coordinate) (second next-coordinate) boxstar))
									(t NIL) ; this line should not be executed 
								)
							) 
							(t NIL)
						)
					)
				)
			)
			(t NIL)
		)
	)
)


; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
; It returns 0 as document required. 
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; This function use recrusion to find the number of Box in the list. It checks on every atom's nubmer. 
; it is admissible heuristic b/c it never overestimates the number of misplaced boxes. 
(defun h1 (s)
	(cond
		((null s) 0)
		((atom s)
			(cond
				((isBox s) 1)
				(t 0)
		))
		(t (+ (h1 (first s)) (h1 (rest s))))
	)
)

; helper function for find_ele 
; finding element on a specific row
(defun helper_find_ele (row v r c)
	(cond 
		((null row) NIL)
		((= v (first row)) (cons (list r c) (helper_find_ele (rest row) v r (+ c 1))))
		(t (helper_find_ele (rest row) v r (+ c 1)))
	)
)

; helper funciont: find all elemennts' location you want and return it as a list, if there is no such 
; element, return NIL 
; s is the state, v is the element you want to find 
(defun find_ele (s v r c)
	(cond
		((null s) NIL)
		(t (append (helper_find_ele (first s) v r 0) (find_ele (rest s) v (+ r 1) 0)))
	)
)

; return the absolute value 
(defun abs (value)
	(cond
		((< value 0) (- 0 value))
		((> value 0) value)
		(t 0)
	)
)

; calculate teh manhattan distance between two points 
(defun manhattan_distance (start end)
	(let 
		(
			(start_r (first start))
			(start_c (second start))
			(end_r (first end))
			(end_c (second end))
		)
		(abs (+ (abs (- end_r start_r)) (abs (- end_c start_c))))
	)
)

; find the smallest distance between a box and its goals 
; box should be a list of a pair of coordinate
; goals should be a list of list of a pair of coordinate
(defun smallest_distance_between (box goals curr_smallest_dis)
	(cond
		((null goals) curr_smallest_dis)
		(t 
			(let*
				((curr_dist (manhattan_distance box (first goals)))) ; store the distance 
				(cond
					((null curr_smallest_dis) (smallest_distance_between box (rest goals) curr_dist)) ; when curr_smallest dis has no value 
					(t 
						(cond
							((<  curr_dist curr_smallest_dis) (smallest_distance_between box (rest goals) curr_dist)) ;if the new distance is smaller than the stored one 
							(t (smallest_distance_between box (rest goals) curr_smallest_dis)) ; otherwise 
						)
					)
				)
			)
		)
	)
)

; sum up all smallest distance 
; boxes and goals should be a list of list of coordiantes 
(defun sum_smallest_distance (boxes goals)
	(cond 
		((null boxes) 0) ; base case for the recursion 
		(t (+ (smallest_distance_between (first boxes) goals NIL) (sum_smallest_distance (rest boxes) goals))) ;add all smallest distance together 
		; for every box, check its distance between it and all goals 
	)
)




; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
; 
; This heruristic functions calculate the sum of distance between each box and its nearest goal
(defun h904801945 (s)
	(+ (sum_smallest_distance (find_ele s 2 0 0) (find_ele s 4 0 0)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3  0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
    (0 2 1 4 0 4 0)
    (0 2 0 4 0 0 0)    
    (3 2 1 1 1 4 0)
    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
