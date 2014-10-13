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
    queue))

(defun as1 (queue new-list)
  (apply-heuristic (cdr queue) new-list #'h1a)
  )

(defun as2 (queue new-list)
  (apply-heuristic (cdr queue) new-list #'h2a)
  )

;; ========================================

;; Heuristics
;; ========================================

; first heuristic - admissible
; based on the number of doubles needed to
; get the current block to the target block
(defun h1 (n1 n2)
  (let ((max1 (grid-max (node-state n1)))
        (max2 (grid-max (node-state n2)))
        (maxdist (log target 2)))
    (let ((dist1 (log max1 2))
          (dist2 (log max2 2)))
      (< (- maxdist dist1) (- maxdist dist2)))))

; second heuristic - admissible
; same as the first heuristic only adds the cost
; already acquired to the current node for A*
(defun h1a (n1 n2)
  (let ((max1 (grid-max (node-state n1)))
        (max2 (grid-max (node-state n2)))
        (maxdist (log target 2)))
    (let ((dist1 (log max1 2))
          (dist2 (log max2 2)))
      (< (+ (- maxdist dist1) (node-cost n1))
         (+ (- maxdist dist2) (node-cost n2))))))

; third heuristic - admissible
; calculates the actual score acquired from doubling
; the current maximum block to the target
; same idea as second heuristic but dominates it
(defun h2a (n1 n2)
 (let ((max1 (grid-max (node-state n1)))
       (max2 (grid-max (node-state n2))))
   (< (+ (expected-cost max1) (node-cost n1))
      (+ (expected-cost max2) (node-cost n2)))))

; fourth heuristic - not admissible
; counts the number of combination moves needed
; assuming on the current maximum block exists 
; in grid
(defun h2 (n1 n2)
  (let ((max1 (grid-max (node-state n1)))
        (max2 (grid-max (node-state n2))))
    (let ((dist1 (/ (- target max1) 2))
          (dist2 (/ (- target max2) 2)))
      (< dist1 dist2))))

;; Helper Functions
;; ========================================

(defun apply-heuristic (queue new-list h)
  (merge-sort (append queue new-list) h))

(defun merge-sort (list cmp)
  (if (small list) list
    (merge-lists
      (merge-sort (left-half list) cmp)
      (merge-sort (right-half list) cmp)
      cmp)))

(defun small (list)
  (or (eq (length list) 0) (eq (length list) 1)))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))

(defun left-half (list)
  (ldiff list (right-half list)))

(defun merge-lists (list1 list2 cmp)
  (merge 'list list1 list2 cmp))

(defun expected-cost (cost)
  (do ((ecost 0 (+ cost ecost)))
    ((>= cost target) ecost)
    (setf cost (* 2 cost))))
