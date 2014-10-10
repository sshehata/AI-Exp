(setf g (GenGrid))
(setf ops '(grid-up grid-down grid-left grid-right))

(defun goalp (g)
  (eql (grid-max g) 16))

(setf prp (make-instance 'search-p :operators ops
                                  :initial g
                                  :goalp #'goalp
                                  :costfun #'+))
(setf sol (general-search prp #'bfs))
