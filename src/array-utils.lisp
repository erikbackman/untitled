(in-package :untitled)

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
