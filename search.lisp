;; Search Strategies
;; ========================================

(defun bf (queue new-list)
  (append (cdr queue) new-list))

(defun df (queue new-list)
  (append new-list (cdr queue)))

(defun gr1 (queue new-list)
  (apply-heuristic (cdr queue) new-list #'h1))

(defun gr2 (queue new-list)
  (apply-heuristic (cdr queue) new-list #'h2))

(defun id (queue new-list)
  (if (not (boundp 'tree-root))
    (progn (setf tree-root (car queue))
           (setf  depth 0)
           (setf stop T)))
  (if (and (not (null new-list))
           (> (node-depth (car new-list)) depth))
    (progn (setf stop nil)
           (setf queue (cdr queue)))
    (setf queue (append new-list (cdr queue))))
  (if (and (null queue) (not stop))
    (progn (setf depth (1+ depth))
           (setf stop T)
           (list tree-root))
    (if (and (null queue) stop)
      (progn (makunbound 'tree-root)
             (makunbound 'depth)
             (makunbound 'stop)
             queue)
      queue)))

;; ========================================

;; Helper Functions
;; ========================================

(defun apply-heuristic (queue new-list h)
  (if (null new-list)
    queue
    (let ((new-queue (insert-sorted queue (car new-list) h)))
      (apply-heuristic new-queue (cdr new-list) h))))

(defun insert-sorted (queue node betterp)
  (if (null queue)
    (list node)
    (if (funcall betterp node (car queue))
      (cons node queue)
      (cons (car queue) (insert-sorted (cdr queue) node betterp)))))

(defun h1 (n1 n2)
  (let ((max1 (grid-max (node-state n1)))
        (max2 (grid-max (node-state n2)))
        (maxdist (log target 2)))
    (let ((dist1 (log max1 2))
          (dist2 (log max2 2)))
      (< (- maxdist dist1) (- maxdist dist2)))))

(defun h2 (n1 n2)
  (let ((max1 (grid-max (node-state n1)))
        (max2 (grid-max (node-state n2))))
    (let ((dist1 (/ (- target max1) 2))
          (dist2 (/ (- target max2) 2)))
      (< dist1 dist2))))
