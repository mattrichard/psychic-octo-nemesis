
; Constants used to indicate which side of the river the conoe is on (for readability)
(defconstant +canoe-left+ 1 "Value of the conoe when it is on left bank.")
(defconstant +conoe-right+ -1 "Value of the conoe when it is on the right bank.")
(defconstant +left-bank+ :left)
(defconstant +right-bank+ :right)

(defun get-m (state bank)
  "Returns the number of missionaries on the given bank"
  (getf (getf state bank) :missionaries))

(defun get-c (state bank)
  "Returns the number of cannibals on the given bank"
  (getf (getf state bank) :cannibals))

(defun is-conoe-left (state)
  "Returns t if the canoe is on the left bank; otherwise, returns nil."
  (= (getf state :canoe) +canoe-left+))

(defun is-conoe-right (state)
  "Returns t if the canoe is on the right bank; otherwise, returns nil."
  (= (getf state :canoe) +canoe-right+))

(defun conoe-state-to-string (state)
  (cond
    ((is-conoe-left state) "left")
    ((is-conoe-right state) "right")
    (t "unknown")))

(defun print-state (state)
  (format t "left side~15tright side~30tcanoe~40tlast move~%")
  (format t "---------~15t----------~30t-----~40t---------~%")
  (format t "~3dM,~3dC~15t ~3dM,~3dC~30t~5a"
            (get-m state +left-bank+)
            (get-c state +left-bank+)
            (get-m state +right-bank+)
            (get-c state +right-bank+)
            (conoe-state-to-string state)))

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

(defun dfs ())

(defun m-c (m c)
  (let ((state (create-state-plist m c)))
    (print-state state)))

(m-c -1 -1)
