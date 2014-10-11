;; =========================================
;; =========== REQUIRED FUNCTIONS ==========
;; =========================================

; search for a solution for the given grid and
; target M using the given strategy
; named Search_ because we can not override the
; predefined search function
(defun Search_ (grid M strategy visualise)
  (setf target M)
  (labels ((goalp (g) (eql (grid-max g) M)))
    (let* ((ops '(grid-up grid-down grid-left grid-right))
           (prp (make-instance 'search-p :operators ops
                                         :initial   grid
                                         :goalp     #'goalp
                                         :costfun   #'+)))
      (multiple-value-bind (sol expand-count sol-cost)
        (general-search prp strategy)
        (if visualise
          (show-solution grid sol #'grid-display))   
        (list sol sol-cost expand-count)))))

;; =========================================
;; ========== REQUIRED DATA TYPES ==========
;; =========================================

; a search tree node.
(defstruct node
  state parent operator depth cost)

; an abstract search problem.
(defclass search-p ()
  ((operators :accessor search-ops      ; a list of functions that can be applied
              :initarg :operators)      ; to a state to acquire the next state.

   (s0        :accessor initial-state   ; the initial state.
              :initarg :initial)

   ; (state-space)                      ; defined implictly by the operators ?

   (goal-test :accessor search-goalp    ; a function to be applied on a search
              :initarg :goalp)          ; node state to check if goal has been reached.

   (path-cost :accessor search-cost     ; a function to calculate the cost of reaching
              :initarg :costfun)        ; a node. Not sure how to abstract this, perhaps
                                        ; we are taking it too literally ?
   ))

; a general search function.
; Input  - prp      : general search problem of class search-p
;          qing-fun : a queueing function of children nodes
;                     the determines the search strategy
; Output - a list of operators to applied on the initial state
;          to reach the goal. returns nil if no solution is found

(defmethod general-search ((prp search-p) qing-fun)
  (let ((root (make-node :state (initial-state prp)
                         :depth 0
                         :cost  0)))
    (multiple-value-bind (goal-node expand-count)
      (search-helper prp (list root) qing-fun)
      (if (not goal-node)
        (format t "No solution was found. ~% Try relaxing your problem constraints.")
        (values (form-solution goal-node nil)
                expand-count
                (node-cost goal-node))))))


;; =========================================

;; Helper Functions
;; =========================================

(defun search-helper (prp queue qing-fun)
  (do ((expand-count 0 (1+ expand-count))
       (q queue (funcall qing-fun 
                         (cdr q)
                         (expand (car q) (search-ops prp)
                                 (search-cost prp)))))
    ((null q) (values nil expand-count))
    (if (funcall (search-goalp prp) (node-state (car q)))
      (return-from search-helper (values (car q) expand-count)))))

(defun expand (head operators costfun)
  (if (null operators)
    nil
    (multiple-value-bind (new-state new-cost) 
      (funcall (car operators) (node-state head))
      (if (null new-state)
        (expand head (cdr operators) costfun)
        (cons (make-node :state new-state
                         :parent head
                         :operator (car operators)
                         :depth (1+ (node-depth head))
                         :cost (funcall costfun (node-cost head) new-cost))
              (expand head (cdr operators) costfun))))))

(defun form-solution (n op-list)
  (if (null (node-parent n))
    op-list
    (form-solution (node-parent n) (cons (node-operator n) op-list))))

(defun show-solution (s0 sol &optional (print-fun #'print))
  (format t "initial ~%")
  (funcall print-fun s0)
  (read-char)
  (loop for op in sol
        do 
        (format t "~A ~%" op)
        (setf s0 (funcall op s0))
        (funcall print-fun s0)
        (read-char)))
