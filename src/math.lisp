(in-package :untitled)

(defun make-identity-matrixf (n)
  (let ((m (make-array (list n n) :initial-element 0.0)))
    (dotimes (i n)
      (setf (aref m i i) 1.0))
    m))

(defun dot (v1 v2)
  (let* ((dim (array-dimension v1 0))
	 (v3 (make-array dim)))
    (dotimes (i dim)
      (setf (aref v3 i) (* (aref v1 i) (aref v2 i))))
    v3))

(defun matrix-mulf (m1 m2)
  (let* ((n (array-dimension m1 1))
	 (m3 (make-array `(,n ,n) :initial-element 0.0)))
    (loop for r from 0 below n do
      (loop for c from 0 below n do
	(loop for i from 0 below n do
	  (incf (aref m3 r c) (* (aref m1 r i) (aref m2 i c))))))
    m3))

(defun tr-rotate (trans angle)
  (let ((mat (make-identity-matrixf 4))
	(cosv (cos angle))
	(sinv (sin angle)))
    (setf
     (aref mat 0 0) cosv
     (aref mat 0 1) (- sinv)
     (aref mat 1 0) cosv
     (aref mat 1 1) sinv)
    (matrix-mulf mat trans)))
