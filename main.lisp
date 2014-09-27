; a search tree node.
(defstruct node
  state parent operator depth cost)

; an abstract search problem.
(defclass search-p
  (operators :accessor search-ops      ; a list of functions that can be applied
             :initarg :operators)      ; to a state to acquire the next state.
  
  (s0        :accessor search-initial  ; the initial state.
             :initarg :initial)

; (state-space)                        ; defined implictly by the operators ?
  
  (goal-test :accessor search-goalp    ; a function to be applied on a search
             :initarg :goalp)          ; node state to check if goal has been reached.
  
  (path-cost :accessor search-cost     ; a function to calculate the cost of reaching
             :initarg :costfun)        ; a node. Not sure how to abstract this, perhaps
                                       ; we are taking it too literally.
  )
