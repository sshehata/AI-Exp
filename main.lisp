;; =========================================
;; =========== REQUIRED FUNCTIONS ==========
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
    (let ((goal-node (search-helper prp (list root) qing-fun)))
       (if (not goal-node)
         (format t "No solution was found. ~% Try relaxing your problem constraints.")
         (form-solution goal-node nil)))))

;; =========================================

;; Helper Functions
;; =========================================

(defun search-helper (prp queue qing-fun)
  (if (null queue)
    nil
    (let ((head (car queue)))
      (if (funcall (search-goalp prp) (node-state head))
        head 
        (search-helper prp
                       (funcall qing-fun 
                                (cdr queue) 
                                (expand head (search-ops prp) (search-cost prp)))
                       qing-fun)))))

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

(defun show-sol (s0 sol &optional (print-fun #'print))
  (format t "initial ~%")
  (funcall print-fun s0)
  (read-char)
  (loop for op in sol
        do 
        (format t "~A ~%" op)
        (setf s0 (funcall op s0))
        (funcall print-fun s0)
        (read-char)))
