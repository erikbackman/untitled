(in-package :untitled)

(defmacro set-indicies (arr &body body)
  `(setf ,@(loop for i from 0 below (length body) by 2
		 for v from 1 by 2
		 append `((aref ,arr ,@(elt body i)) ,(elt body v)))))

(defmacro !x (vec)
  `(aref ,vec 0))

(defmacro !y (vec)
  `(aref ,vec 1))

(defmacro !z (vec)
  `(aref ,vec 2))

(defmacro with-elements ((x y z) vec &body body)
  (let ((a (gensym)))
    `(let* ((,a ,vec)
	    (,x (aref ,a 0))
	    (,y (aref ,a 1))
	    (,z (aref ,a 2)))
       ,@body)))
