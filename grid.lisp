;; =========================================
;; =========== REQUIRED FUNCTIONS ==========
;; =========================================

(defun GenGrid ()
  (let ((size 4)
        (g (make-instance 'grid :size 4)))
    (grid-spwanblock g (random size) (random size))
    (do ((x (random size) (random size))
         (y (random size) (random size)))
      ((grid-emptyblockp g x y) 
       (grid-spwanblock g x y) 
       g))))

;; =========================================

;; Grid Class
;; =========================================

(defclass grid () 
  ((blocks  :accessor grid-blocks)

   (size    :initarg :size 
            :initform 4 :accessor grid-size)

   (maximum :accessor grid-max
            :initform 0)))

(defmethod initialize-instance :after ((g grid) &KEY)
  (let ((size (grid-size g))) 
    (setf (grid-blocks g) (make-array (list size size) :initial-element 0))))

(defmethod grid-display ((g grid))
  (let ((blocks (grid-blocks g)))
    (do ((i 0 (+ i 1)))
      ((>= i (grid-size g)) nil)
      (do ((j 0 (+ j 1)))
        ((>= j (grid-size g)) nil)
        (format t "~5A" (if (eql (aref blocks i j) 0) "*" (aref blocks i j))))
      (format t "~%"))))

(defmethod grid-spwanblock ((g grid) x y)
  (let ((blocks (grid-blocks g)))
    (setf (aref blocks x y) 2)))

(defmethod grid-emptyblockp ((g grid) x y)
  (eql (aref (grid-blocks g) x y) 0))

(defmethod grid-update ((g grid))
  (let ((border (- (grid-size g) 1)))
    (if (grid-emptyblockp g 0 0)
      (grid-spwanblock g 0 0)
      (if (grid-emptyblockp g 0 border)
        (grid-spwanblock g 0 border)
        (if (grid-emptyblockp g border border)
          (grid-spwanblock g border border)
          (if (grid-emptyblockp g border 0)
            (grid-spwanblock g border 0)
            nil))))))

(defmethod grid-find-max ((g grid))
  (do ((i 0 (1+ i)))
    ((>= i (grid-size g)) nil)
    (do ((j 0 (1+ j)))
      ((>= j (grid-size g)) nil)
      (let ((el (aref (grid-blocks g) i j)))
        (if (> el (grid-max g))
          (setf (grid-max g) el))))))

(defmethod eqlp ((g1 grid) (g2 grid))
  (equalp (grid-blocks g1) (grid-blocks g2)))

;; Operators
;; ==========================================

(defmethod grid-right ((g grid))
  (defparameter *cost* 0)
  (let ((g2 (make-instance 'grid :size (grid-size g)))) 
    (setf (grid-blocks g2) (toarray (mapcar #'collect-right (listify2d (grid-blocks g)))))
    (if (eqlp g2 g)
      (return-from grid-right nil))
    (grid-update g2)
    (grid-find-max g2)
    (values-list (list g2 *cost*))))

(defmethod grid-left ((g grid))
  (defparameter *cost* 0)
  (let ((g2 (make-instance 'grid :size (grid-size g)))) 
    (setf (grid-blocks g2) (toarray (mapcar #'collect-left (listify2d (grid-blocks g)))))
    (setf (grid-max g2) (grid-max g))
    (if (eqlp g2 g)
      (return-from grid-left nil))
    (grid-update g2)
    (grid-find-max g2)
    (values-list (list g2 *cost*))))

(defmethod grid-up ((g grid))
  (defparameter *cost* 0)
  (let ((g2 (make-instance 'grid :size (grid-size g)))) 
    (setf (grid-blocks g2) (toarray (rotate (mapcar #'collect-left (rotate (listify2d (grid-blocks g)))))))
    (if (eqlp g2 g)
      (return-from grid-up nil))
    (grid-update g2)
    (grid-find-max g2)
    (values-list (list g2 *cost*))))

(defmethod grid-down ((g grid))
  (defparameter *cost* 0)
  (let ((g2 (make-instance 'grid :size (grid-size g)))) 
    (setf (grid-blocks g2) (toarray (rotate (mapcar #'collect-right (rotate (listify2d (grid-blocks g)))))))
    (if (eqlp g2 g)
      (return-from grid-down nil))
    (grid-update g2)
    (grid-find-max g2)
    (values-list (list g2 *cost*))))

;; Helper Functions
;; ==========================================

(defun rotate (l)
  (apply #'mapcar #'list l))

(defun collect-left (row)
  (let ((len (length row))
        (merged (pairwise-merge row)))
    (append merged (make-list (- len (length merged)) :initial-element 0))))

(defun collect-right (row)
  (let ((len (length row))
        (merged (reverse (pairwise-merge (reverse row)))))   ; could use just one reverse
    (append (make-list (- len (length merged)) :initial-element 0) merged)))

(defun pairwise-merge (l)
  (if (null l)
    nil
    (let ((x1 (car l))
          (x2 (car (cdr l))))
      (if (zerop x1)
        (pairwise-merge (cdr l))
        (if (and (not (null x2))
                 (zerop x2))
          (pairwise-merge (cons x1 (cdr (cdr l))))
          (if (eql x1 x2)
            (progn (setf *cost* (+ *cost* (* 2 x1)))
                   (cons (* 2 x1) (pairwise-merge (cdr (cdr l)))))
            (cons x1 (pairwise-merge (cdr l)))))))))

(defun listify2d (x)
  (loop for i from 0 to (- (array-dimension x 0) 1)
        collect (loop for j from 0 to (- (array-dimension x 1) 1)
                      collect (aref x i j))))

(defun toarray (x) 
  (make-array (list (length x) (length (car x))) :initial-contents x))
