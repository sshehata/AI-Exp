(defclass grid () 
  ((blocks :accessor grid-blocks)
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
