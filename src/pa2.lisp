;;;; Programming Assignment #2: Functional Programming in Lisp
;;;;                            Missionaries and Cannibals Puzzle
;;;;
;;;; Author:	Matt Richard
;;;; Date:	Sun Apr 14, 2013
;;;; Professor:	Dr. John Weiss
;;;; Course:	CSC 461
;;;; Location:	McLaury 313
;;;;
;;;; Description:
;;;;
;;;; Three missionaries and three cannibals find themselves on one bank of a
;;;; piranha-infested river with a canoe that holds only two persons. How can
;;;; they safely cross the river in such a way that the number of missionaries
;;;; on either bank is never less than the number of cannibals? Write a program
;;;; in Lisp to help everyone cross the river safely.
;;;;
;;;; This classic AI problem assumes exactly three missionaries and three
;;;; cannibals, but your program will allow the user to specify any number of
;;;; missionaries and cannibals. The program is run by typing:
;;;;
;;;;	(m-c m c)
;;;;
;;;; where m is the number of missionaries and c is the numb er of cannibals.
;;;; Note that not all problem instances are solvable. It should be possible to
;;;; run this program more than once in a Lisp session.
;;;;
;;;; Program output is a nicely formatted set of statements describing the
;;;; current state (how many cannibals and missionaries are on each bank, and
;;;; where the canoe is) and the last move made. For consistency, let us assume
;;;; that the goal is to move everyone from the left bank to the right bank.
;;;; Here is sample output for (m-c 3 3). You do not have to use exactly the
;;;; same format.
;;;;
;;;; 	3 Missionaries and 3 Cannibals:
;;;;
;;;; 	left bank	right bank	canoe	last move
;;;;	---------	----------	-----	---------
;;;;	3 M, 3 C	0 M, 0 C	left	start position
;;;;	2 M, 2 C	1 M, 1 C	right	move 1 M, 1 C left to right
;;;;	    .		    .		  .	     .
;;;;	    .		    .		  .	     .
;;;;	    .		    .		  .	     .
;;;;
;;;; The high-level symbolic approach to problem-solving in AI is known as the
;;;; state space approach, and involves graph search. In this approach,
;;;; successor states are generated from the start state. One or more of these
;;;; successors are selected for exploration in the next iteration, new
;;;; successors are generated, and eventually a solution path is determined
;;;; from the start state to the goal state in the state space graph.
;;;;
;;;; A variety of search strategies have been developed to explore the state
;;;; space. Exhaustive search strategies eventually explore all possible
;;;; successor states en route to finding a solution path. Depth-first search
;;;; (DFS) is an exhaustive search technique that is most easily described
;;;; recursively:
;;;;
;;;;	if ( current_state == goal_state )
;;;;		return SUCCESS;
;;;;
;;;;	for each unexplored successor of the current state
;;;;		if ( DFS( successor ) == SUCCESS )
;;;;			return SUCCESS;
;;;;
;;;;	return FAILURE;
;;;;
;;;; DFS explores potential solution paths as deeply as possible, until it
;;;; reaches a goal (success), or hits a dead end (no more unexplored successor
;;;; nodes) and must backtrack. Recursion is an elegant way to handle the
;;;; backtracking. Once a goal state is reached, the path can be traced back to
;;;; the start state to recover the sequence of solution steps.
;;;;
;;;; Usage:
;;;; 
;;;; 1. From a terminal:
;;;;
;;;;	$ clisp pa2.lisp M C
;;;;
;;;; 2. From a Lisp session:
;;;;
;;;;	(m-c m c)
;;;;
;;;; Bugs: None


;;; Constants used to indicate which side of the river the canoe is on
(defconstant +left-bank+ :left)
(defconstant +right-bank+ :right)
(defconstant +max-canoe-count+ 2 "Maximum number of missionaries or cannibals
that the canoe can hold.")


(defun get-other-bank (bank)
  "Returns the bank opposite to the given bank, or nil if the given bank is 
unknown."
  (cond
    ((eql bank +left-bank+) +right-bank+)
    ((eql bank +right-bank+) +left-bank+)
    (t nil)))


(defmacro get-bank (state bank)
  "Macro for getting the given bank from the state plist."
  `(getf ,state ,bank))

(defmacro get-m (state bank)
  "Macro for getting the number of missionaries on the given bank."
  `(getf (get-bank ,state ,bank) :missionaries))

(defmacro get-c (state bank)
  "Macro for getting the number of cannibals on the given bank."
  `(getf (get-bank ,state ,bank) :cannibals))

(defmacro get-canoe (state)
  "Macro for getting the state of the canoe."
  `(getf ,state :canoe))

(defmacro get-canoe-bank (state)
  "Macro for getting the current bank the canoe is at."
  `(getf (get-canoe ,state) :bank))

(defmacro get-canoe-seats (state)
  "Macro for getting the seats of the canoe."
   `(getf (get-canoe ,state) :seats))


(defun get-canoe-count (state)
  "Returns the number of missionaries and cannibals currently in the canoe."
  (length (get-canoe-seats state)))

(defun is-canoe-empty (state)
  "Returns t if there are no missionaries nor cannibals in the canoe; 
otherwise, returns nil."
  (zerop (get-canoe-count state)))

(defun is-canoe-full (state)
  "Returns t if there are 2 people (missionaries or cannibals on the canoe; 
otherwise, returns nil."
  (= (get-canoe-count state) +max-canoe-count+))

(defun is-canoe-left (state)
  "Returns t if the canoe is on the left bank; otherwise, returns nil."
  (eql (get-canoe-bank state) +left-bank+))

(defun is-canoe-right (state)
  "Returns t if the canoe is on the right bank; otherwise, returns nil."
  (eql (get-canoe-bank state) +right-bank+))

;(defun canoe-state-to-string (state)
;  (cond
;    ((is-canoe-left state) "left")
;    ((is-canoe-right state) "right")
;    (t "unknown")))

(defun load-canoe (person &optional (num 1)))

(defun unload-canoe (person &optional (num 1)))


(defun dec-m (state bank)
  "Decrements the number of missionaries on the given bank by 1."
  (decf (get-m state bank)))

(defun dec-c (state bank)
  "Decrements the number of cannibals on the given bank by 1."
  (decf (get-c state bank)))

(defun inc-m (state bank)
  "Increments the number of missionaries on the given bank by 1."
  (incf (get-m state bank)))

(defun inc-c (state bank)
  "Increments the number of cannibals on the given bank by 1."
  (incf (get-c state bank)))


(defun move-canoe (state)
  "Moves the canoe from the current bank to the other."
  (setf (get-canoe-bank state) (get-other-bank (get-canoe-bank state))))
;  "Moves the canoe from the current bank to the other"
;  (setf (get-canoe state) (- (get-canoe state))))

(defun move-one-m (state)
  "Moves one missionary from the current bank to the other."
  (let* ((current-bank (get-canoe-bank state)
         (other-bank (get-other-bank current-bank))))
    (when (> (get-m state current-bank) 0)
      (dec-m state current-bank)
      (inc-m state other-bank)
      (move-canoe state))))

(defun move-two-m (state)
  "Moves two missionaries from the current bank to the other."
  (let* ((current-bank (get-canoe-bank state)
         (other-bank (get-other-bank current-bank))))
    (when (> (get-m state current-bank) 1)
      (dec-m state current-bank)
      (dec-m state current-bank)
      (inc-m state other-bank)
      (inc-m state other-bank)
      (move-canoe state))))

(defun move-m-c (state)
  "Moves one missionary and one cannibal from the current bank to the other."
  (let* ((current-bank (get-canoe-bank state)
         (other-bank (get-other-bank current-bank))))
    (when (and (> (get-c state current-bank) 0)
               (> (get-m state current-bank) 0))
      (dec-c state current-bank)
      (dec-m state current-bank)
      (inc-c state other-bank)
      (inc-m state other-bank)
      (move-canoe state))))

(defun move-one-c (state)
  "Moves one cannibal from the current bank to the other."
  (let* ((current-bank (get-canoe-bank state)
         (other-bank (get-other-bank current-bank))))
    (when (> (get-c state current-bank) 0)
      (dec-c state current-bank)
      (inc-c state other-bank)
      (move-canoe state))))

(defun move-two-c (state)
  "Moves two cannibals from the current bank to the other."
  (let* ((current-bank (get-canoe-bank state)
         (other-bank (get-other-bank current-bank))))
    (when (> (get-c state current-bank) 1)
      (dec-c state current-bank)
      (dec-c state current-bank)
      (inc-c state other-bank)
      (inc-c state other-bank)
      (move-canoe state))))


(defun print-header ()
  (format t "left side~15tright side~30tcanoe~40tlast move~%")
  (format t "---------~15t----------~30t-----~40t---------~%"))

(defun print-state (state)
  (format t "~3dM,~3dC~15t ~3dM,~3dC~30t~5a~%"
            (get-m state +left-bank+)
            (get-c state +left-bank+)
            (get-m state +right-bank+)
            (get-c state +right-bank+)
            (get-canoe-bank state)))


(defun create-m-c-plist (&key (m 0) (c 0))
  "Returns a plist containing the keys MISSIONARIES and CANNIBALS and values 
:m and :c, respectively."
  (list :missionaries m :cannibals c))

(defun create-bank-plist (bank-key &key (m 0) (c 0))
  "Returns a plist with the key as bank-key and the value a plist created from 
create-m-c-plist."
  (list bank-key (create-m-c-plist :m m :c c)))

(defun create-canoe-plist (&optional (canoe-bank +left-bank+))
  (list :canoe (list :bank canoe-bank :seats ())))

(defun create-state-plist (m c)
  "Generates a plist used as the data structure for solving the missionaries 
and cannibals problem. The state is initialized as:
(:LEFT (:MISSIONAIRES m :CANNIBALS c) :RIGHT (:MISSIONARIES 0 :CANNIBALS 0)
:CANOE (:BANK +left-bank+ :SEATS ())."
  (append (create-bank-plist +left-bank+ :m m :c c)
          (create-bank-plist +right-bank+)
          (create-canoe-plist)))


(defun success (state)
  ;; Success if the canoe, all missionaries, and all cannibals are on the
  ;; right bank (i.e. no missionaries nor cannibals are on the left bank)
  (and (= (get-m state +left-bank+) 0)
       (= (get-c state +left-bank+) 0)
       (is-canoe-right state)))

(defun generate-next-states (state)
  ;TODO: write this
  )

(defun dfs (state success-form generator-form)
  ;; Check for success
  (if (funcall success-form state)
      (return t))
  (dolist (next-state (funcall generator-form state))
    (if (dfs next-state #'success-form #'generator-form)
        (return t)))
  nil)


(defun m-c (m c)
  (let ((state (create-state-plist m c)))
    (print-header)
    ;(print state)))
    (print-state state)))
    ;(inc-m state +left-bank+)
    ;(print-state state)))
    ;(inc-c state +left-bank+)
    ;(print-state state)
    ;(inc-m state +right-bank+)
    ;(print-state state)
    ;(inc-c state +right-bank+)
    ;(print-state state)))
    ;(dfs state #'success #'generate-next-states)))


(m-c -1 -1)
