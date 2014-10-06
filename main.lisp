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
    (let ((goal-node (search-helper (prp (list root) qing-fun))))
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
      (if (funcall (search-goalp prp) head)
        head 
        (search-helper prp
                       (funcall qing-fun 
                                (cdr queue) 
                                (expand head (search-ops prp) (search-cost prp)))
                       qing-fun)))))

(defun expand (head operators costfun)
  (mapcar #'(lambda (op) (let ((new-cost 0)
                               (new-state nil))
                           (setf (values new-state new-cost) (funcall op (node-state head)))
                           (make-node :state    new-state
                                      :parent   head
                                      :operator op
                                      :depth    (+ (node-depth head) 1)
                                      :cost     (funcall costfun (node-cost head) new-cost))))
          operators))

(defun form-solution (node op-list)
  (if (null (node-parent n))
    op-list
    (form-solution (node-parent n) (cons (node-operator n) op-list))))
