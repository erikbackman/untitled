(in-package :untitled)

(defmacro set-indicies (arr &body body)
  `(setf ,@(loop for i from 0 below (length body) by 2
		 for v from 1 by 2
		 append `((aref ,arr ,@(elt body i)) ,(elt body v)))))
