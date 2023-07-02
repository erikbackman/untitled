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

(defun tr-rotate (trans angle x y z)
  (let ((mat (make-identity-matrixf 4))
	(cosv (cos angle))
	(sinv (sin angle)))
    (setf
     (aref mat 0 0) (+ cosv (* (expt x 2) (- 1 cosv)))
     (aref mat 0 1) (- (* x y (- 1 cosv)) (* z sinv))
     (aref mat 0 2) (+ (* x z (- 1 cosv)) (* y sinv))
     (aref mat 1 0) (- (* y x (- 1 cosv)) (* z sinv))
     (aref mat 1 1) (+ cosv (* (expt y 2) (- 1 cosv)))
     (aref mat 1 2) (- (* y z (- 1 cosv)) (* x sinv))
     (aref mat 2 0) (- (* z x (- 1 cosv)) (* y sinv))
     (aref mat 2 1) (+ (* z y (- 1 cosv)) (* x sinv))
     (aref mat 2 2) (+ cosv (* (expt z 2) (- 1 cosv))))
    (matrix-mulf mat trans)))
