
; Constants used to indicate which side of the river the canoe is on (for readability)
(defconstant +canoe-left+ 1 "Value of the canoe when it is on left bank.")
(defconstant +canoe-right+ -1 "Value of the canoe when it is on the right bank.")
(defconstant +left-bank+ :left)
(defconstant +right-bank+ :right)

(defun get-canoe-bank (state)
  "Maps the canoe's current state into the proper bank constant (i.e. +left-bank+ or +right-bank+, or nil if unknown)."
  (cond
    ((is-canoe-left state) +left-bank+)
    ((is-canoe-right state) +right-bank+)
    (t nil)))

(defun get-other-bank (bank)
  "Returns the bank opposite to the given bank, or nil if the bank is unknown."
  (cond
    ((= bank +left-bank+) +right-bank+)
    ((= bank +right-bank+) +left-bank+)
    (t nil)))

(defmacro get-m (state bank)
  "Returns the number of missionaries on the given bank."
  `(getf (getf ,state ,bank) :missionaries))

(defmacro get-c (state bank)
  "Returns the number of cannibals on the given bank."
  `(getf (getf ,state ,bank) :cannibals))

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
  "Moves the canoe from the current bank to the other"
  (setf (getf state :canoe) (- (getf state :canoe))))

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

(defun is-canoe-left (state)
  "Returns t if the canoe is on the left bank; otherwise, returns nil."
  (= (getf state :canoe) +canoe-left+))

(defun is-canoe-right (state)
  "Returns t if the canoe is on the right bank; otherwise, returns nil."
  (= (getf state :canoe) +canoe-right+))

(defun canoe-state-to-string (state)
  (cond
    ((is-canoe-left state) "left")
    ((is-canoe-right state) "right")
    (t "unknown")))

(defun print-state (state)
  (format t "left side~15tright side~30tcanoe~40tlast move~%")
  (format t "---------~15t----------~30t-----~40t---------~%")
  (format t "~3dM,~3dC~15t ~3dM,~3dC~30t~5a~%"
            (get-m state +left-bank+)
            (get-c state +left-bank+)
            (get-m state +right-bank+)
            (get-c state +right-bank+)
            (canoe-state-to-string state)))

(defmacro create-m-c-plist (m c)
  "Generates a plist with properties 'missionaries' and 'cannibals' with the values of m and c, respectively."
  `(list :missionaries ,m :cannibals ,c))

(defmacro create-bank-plist (bank-name m c)
  "Generates a plist with the value of bank being the property whose value is a plist created from create-m-c-plist."
  `(list ,bank-name (create-m-c-plist ,m ,c)))

(defmacro create-state-plist (m c)
  `(append (create-bank-plist +left-bank+ m c)
           (create-bank-plist +right-bank+ 0 0)
           (list :canoe +canoe-left+)))

(defun dfs (state)
  (if (and (= (get-m state +left-bank+) 0)
           (= (get-c state +left-bank+) 0)
           (is-canoe-right state)) ; TODO this might not be needed
    (return t)))
;  (let (prev-state state)
;    (when (is-canoe-left state)
;      (when (> (get-c state +left-bank+) 0)

(defun m-c (m c)
  (let ((state (create-state-plist m c)))
    (print-state state)
    (dec-m state +left-bank+)
    (print-state state)
    (dec-m state +right-bank+)
    (print-state state)
    (dec-c state +left-bank+)
    (print-state state)
    (dec-c state +right-bank+)
    (print-state state)
    (move-canoe state)
    (print-state state)))

(m-c -1 -1)
