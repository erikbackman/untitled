(in-package :g3d)

(defmacro set-indicies (arr &body body)
  `(setf ,@(loop for i from 0 below (length body) by 2
		 for v from 1 by 2
		 append `((aref ,arr ,@(elt body i)) ,(elt body v)))))

(defun vec-x (vec) (aref vec 0))

(defun vec-y (vec) (aref vec 1))

(defun vec-z (vec) (aref vec 2))

(defun vec-w (vec) (aref vec 3))

(defun (setf vec-x) (value vec)
  (setf (aref vec 0) value))

(defun (setf vec-y) (value vec)
  (setf (aref vec 1) value))

(defun (setf vec-z) (value vec)
  (setf (aref vec 2) value))

(defmacro with-vec3 ((x y z) vec &body body)
  (let ((a (gensym)))
    `(let* ((,a ,vec))
       (declare (sb-pcl::%variable-rebinding a vec))
       (symbol-macrolet ((,x (aref ,a 0))
			 (,y (aref ,a 1))
			 (,z (aref ,a 2)))
	 ,@body))))

(defmacro with-vec4 ((x y z w) vec &body body)
  (let ((a (gensym)))
    `(let* ((,a ,vec))
       (declare (sb-pcl::%variable-rebinding a vec))
       (symbol-macrolet ((,x (aref ,a 0))
			 (,y (aref ,a 1))
			 (,z (aref ,a 2))
			 (,w (aref ,a 3)))
	 ,@body))))

(defun array2->array1 (arr)
  (destructuring-bind (n m) (array-dimensions arr)
    (let ((v (make-array (* n m))))
      (loop for i from 0 below n do
	(loop for j from 0 below m do
	  (setf (aref v (+ (* i m) j)) (aref arr i j))))
      v)))

(defun index1->index2 (i ncols)
  (list (floor (/ i ncols)) (mod i ncols)))

(defun index2->index1 (row col ncols)
  (+ (* row ncols) col))

(defun array-slice (arr row)
  (make-array (array-dimension arr 1)
	      :adjustable t
	      :fill-pointer 4
	      :displaced-to arr
	      :displaced-index-offset (* row (array-dimension arr 1))))

(defun make-row-resizeable-array (rows max-columns)
  "Returns an array of length ROWS containing arrays
of length MAX-COLUMNS, but with a fill pointer initially
set to 0."
  (make-array
   rows
   :initial-contents
   (loop for i from 0 below rows
         collect (make-array max-columns
			     :fill-pointer 0))))

(defun vec4 (&optional (x 0) (y 0) (z 0) (w 1))
  (make-array 4 :initial-contents `(,x ,y ,z ,w) :fill-pointer 4 :adjustable t))
