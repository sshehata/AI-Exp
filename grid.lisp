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
       (grid-display g) 
       g))))

;; =========================================

;; Grid Class
;; =========================================

(defclass grid () 
  ((locks :accessor grid-blocks)
   (size :initarg :size :initform 4 :accessor grid-size)))

(defmethod initialize-instance :after ((g grid) &KEY)
  (let ((size (grid-size g))) 
    (setf (grid-blocks g) (make-array (list size size) :initial-element 0))))

(defmethod grid-display ((g grid))
  (let ((blocks (grid-blocks g)))
    (do ((i 0 (+ i 1)))
      ((>= i (grid-size g)) nil)
      (do ((j 0 (+ j 1)))
        ((>= j (grid-size g)) nil)
        (format t "~A " (if (eql (aref blocks i j) 0) "*" (aref blocks i j))))
      (format t "~%"))))

(defmethod grid-spwanblock ((g grid) x y)
  (let ((blocks (grid-blocks g)))
    (setf (aref blocks x y) 2)))

(defmethod grid-emptyblockp ((g grid) x y)
  (eql (aref (grid-blocks g) x y) 0))

;; Operators
;; ==========================================

(defmethod grid-right ((g grid))
  (setf (grid-blocks g) (toarray (mapcar #'collect-right (listify2d (grid-blocks g))))))

(defmethod grid-left ((g grid))
  (setf (grid-blocks g) (toarray (mapcar #'collect-left (listify2d (grid-blocks g))))))

(defmethod grid-up ((g grid))
  (setf (grid-blocks g) 
        (toarray (rotate (mapcar #'collect-left (rotate (listify2d (grid-blocks g))))))))

(defmethod grid-down ((g grid))
  (setf (grid-blocks g)
        (toarray (rotate (mapcar #'collect-right (rotate (listify2d (grid-blocks g))))))))

;; Helper Functions
;; ==========================================

(defun rotate (l)
  (apply #'mapcar #'list l))

(defun collect-left (row)
  (let ((len (length row))
        (merged (pairwise-merge (reverse row))))
    (append merged (make-list (- len (length merged)) :initial-element 0))))

(defun collect-right (row)
  (let ((len (length row))
        (merged (reverse (pairwise-merge (reverse row)))))
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
            (cons (* 2 x1) (pairwise-merge (cdr (cdr l))))
            (cons x1 (pairwise-merge (cdr l)))))))))

(defun listify2d (x)
  (loop for i from 0 to (- (array-dimension x 0) 1)
        collect (loop for j from 0 to (- (array-dimension x 1) 1)
                      collect (aref x i j))))

(defun toarray (x) 
  (make-array (list (length x) (length (car x))) :initial-contents x))
