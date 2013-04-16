;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; NOTE:
;;;;
;;;; Program does not work. The state generator is not working as planned.
;;;; The issue seems to be with modifying the state plist directly, rather
;;;; than creating a new list, which is causing side effects when attempting
;;;; to copy the list.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;	(m-c m c)



(defconstant +left-bank+ :left)
(defconstant +right-bank+ :right)
(defconstant +max-canoe-count+ 2 "Maximum number of missionaries or cannibals
that the canoe can hold.")

(defun get-other-bank (bank)
  "Returns the bank opposite to the given bank, or nil if the given bank is 
unknown."
  (cond
    ((eql bank +left-bank+) +right-bank+)
    ((eql bank +right-bank+) +left-bank+)))



;;; Below, are functions used for creating and initializing the data structure
;;; used in solving the missionaries and cannibals problem. Most of the
;;; functions/macros following these functions are provided to allow easy
;;; modification of the state (data structure).
;;;
;;; For this problem, the state plist is laid out and initialized as follows:
;;; 
;;;  (:LEFT  (:MISSIONARIES m
;;;           :CANNIBALS    c)
;;;   :RIGHT (:MISSIONARIES 0
;;;           :CANNIBALS    0)
;;;   :CANOE (:BANK         +left-bank+
;;;           :SEATS        ())
;;;
;;; where m and c are the given number of missionaries and cannibals to solve
;;; the problem for.

(defun create-m-c-plist (&key (m 0) (c 0))
  "Returns a plist containing the keys MISSIONARIES and CANNIBALS and values 
:m and :c, respectively."
  (list :missionaries m :cannibals c))

(defun create-bank-plist (bank-key &key (m 0) (c 0))
  "Returns a plist with the key as bank-key and the value a plist created from 
create-m-c-plist."
  (list bank-key (create-m-c-plist :m m :c c)))

(defun create-canoe-plist (&optional (canoe-bank +left-bank+))
  "Returns a plist for use in initializing the canoe for the state."
  (list :canoe (list :bank canoe-bank :seats ())))

(defun create-state-plist (m c)
  "Returns a plist that is used as the data structure for solving the 
missionaries and cannibals problem."
  (append (create-bank-plist +left-bank+ :m m :c c)
          (create-bank-plist +right-bank+)
          (create-canoe-plist)))



;;; The following macros are provided to allow easy modification of the state
;;; plist. If it weren't for these macros, various functions (e.g. the functions
;;; inc-m, inc-c, dec-m, dec-c defined below) would not modify the plist but
;;; instead create a new plist, which was undesireable.

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



;;; The functions defined below extrapolate the canoe's properties in
;;; relation to the given state.

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



;;; Functions provided for easily incrementing or decrementing the number
;;; of missionaries or cannibals on the current or given bank by one.

(defun dec-m (state &optional (bank (get-canoe-bank state)))
  "Decrements the number of missionaries on the given bank by 1."
  (decf (get-m state bank)))

(defun dec-c (state &optional (bank (get-canoe-bank state)))
  "Decrements the number of cannibals on the given bank by 1."
  (decf (get-c state bank)))

(defun inc-m (state &optional (bank (get-canoe-bank state)))
  "Increments the number of missionaries on the given bank by 1."
  (incf (get-m state bank)))

(defun inc-c (state &optional (bank (get-canoe-bank state)))
  "Increments the number of cannibals on the given bank by 1."
  (incf (get-c state bank)))



(defun load-canoe (state m-or-c &optional (num 1))
  "Loads num number of missionaries or cannibals (specified by m-or-c) from
the current bank onto the canoe."
  ;; Only load the canoe if num is positive and adding num missionaries or
  ;; cannibals won't overflow the canoe
  (when (and (plusp num)
             (<= (+ num (get-canoe-count state)) +max-canoe-count+))
    (let ((dec-func))
      (cond
        ((eql m-or-c :missionaries) (setf dec-func #'dec-m))
        ((eql m-or-c :cannibals) (setf dec-func #'dec-c)))
      (unless (not dec-func)
        ;; Remove num missionaries and cannibals from the current bank and
        ;; put them on the canoe
        (dotimes (i num)
          (funcall dec-func state (get-canoe-bank state))
          (setf (get-canoe-seats state)
                (append (get-canoe-seats state) (list m-or-c))))))))

(defun unload-canoe (state m-or-c &optional (num 1))
  "Unloads num number of missionaries or cannibals (specified by m-or-c) from
the canoe onto the current bank."
  ;; Only load the canoe if num is positive and adding num missionaries or
  ;; cannibals won't overflow the canoe
  (when (and (plusp num)
             (> (- (get-canoe-count state) num) 0))
    (let ((inc-func))
      (cond
        ((eql m-or-c :missionaries) (setf inc-func #'dec-m))
        ((eql m-or-c :cannibals) (setf inc-func #'dec-c)))
      (unless (not inc-func)
        ;; Remove num missionaries or cannibals from the canoe
        (setf (get-canoe-seats state)
              (remove m-or-c (get-canoe-seats state) :count num))
        ;; Put num missionaries or cannibals on the current bank
        (dotimes (i num)
          (funcall inc-func state (get-canoe-bank state)))))))



(defun move-canoe (state)
  "Moves the canoe from the current bank to the other."
  (setf (get-canoe-bank state) (get-other-bank (get-canoe-bank state))))



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


(defun is-valid-state (state)
  (format t "Checking if state is valid:~%")
  (format t "  ~a~%" state)
  (and (>= (get-m state +left-bank+) 0)
       (>= (get-c state +left-bank+) 0)
       (>= (get-m state +right-bank+) 0)
       (>= (get-c state +right-bank+) 0)
       (if (> (get-m state +left-bank+) 0)
         (>= (get-m state +left-bank+) (get-c state +left-bank+))
         t)
       (if (> (get-m state +right-bank+) 0)
         (>= (get-m state +right-bank+) (get-c state +right-bank+))
         t)
       (>= (get-canoe-count state) 0)
       (<= (get-canoe-count state) +max-canoe-count+)))



(defun success (state)
  "Success state for the dfs function."
  ;; Success if the canoe, all missionaries, and all cannibals are on the
  ;; right bank (i.e. no missionaries nor cannibals are on the left bank)
  (and (= (get-m state +left-bank+) 0)
       (= (get-c state +left-bank+) 0)
       (is-canoe-right state)))

(defun generate-next-states (state)
  (format t "Generating next states~%")
  (let ((next-states ())
        (current-bank (get-canoe-bank state))
        (temp-state (copy-list state)))
    (cond
      ((is-canoe-left state)
       (format t "Canoe is left~%")
       (cond
         ((is-canoe-full state)
          (format t "Canoe is full~%")
          (move-canoe temp-state)
          (setf next-states (cons temp-state next-states)))
         ((is-canoe-empty state)
          (format t "Canoe is empty~%")

          (load-canoe temp-state ':missionaries 2)
          (if (is-valid-state temp-state)
            (setf next-states (cons temp-state next-states)))

          (setf temp-state state)
          (load-canoe temp-state ':cannibals 2)
          (if (is-valid-state temp-state)
            (setf next-states (cons temp-state next-states)))

          (setf temp-state state)
          (load-canoe temp-state ':missionaries)
          (load-canoe temp-state ':cannibals)
          (if (is-valid-state temp-state)
            (setf next-states (cons temp-state next-states)))

          (setf temp-state state)
          (load-canoe temp-state ':missionaries)
          (if (is-valid-state temp-state)
            (setf next-states (cons temp-state next-states)))

          (setf temp-state state)
          (load-canoe temp-state ':cannibals)
          (if (is-valid-state temp-state)
            (setf next-states (cons temp-state next-states))))))
      ((is-canoe-right state)
       (format t "Canoe is right~%")))
    next-states))

(defun dfs (state success-form generator-form)
  ;; Check for success
  (if (funcall success-form state)
      (return t))
  ;; Recursive call for all valid next states
  (dolist (next-state (funcall generator-form state))
    (format t "~a~%" next-state)
    (if (dfs next-state success-form generator-form)
        (return t)))
  nil)



(defun m-c (m c)
  (let ((state (create-state-plist m c)))
     (format t "Initial state: ~a~%" state)
;    (print-header)
;    (print-state state)

;    (inc-m state +left-bank+)
;    (print-state state)
;    (inc-c state +left-bank+)
;    (print-state state)
;    (inc-m state +right-bank+)
;    (print-state state)
;    (inc-c state +right-bank+)
;    (print-state state)

;    (load-canoe state ':missionaries 2)
;    (print-state state)

;    (move-canoe state)
;    (print-state state)

;    (unload-canoe state ':missionaries 1)
;    (print-state state)
;    (print state)))
    (if (dfs state #'success #'generate-next-states)
      (format t "Success"))))

;(m-c 1 1)
