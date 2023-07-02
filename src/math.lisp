(in-package :untitled)

(defun make-identity-matrixf (n)
  (let ((m (make-array (list n n) :initial-element 0.0)))
    (dotimes (i n)
      (setf (aref m i i) 1.0))
    m))

(defun dot (v1 v2)
  (loop for i from 0 below (array-dimension v1 0)
	sum (* (aref v1 i) (aref v2 i))))

(defun matrix-mulf (m1 m2)
  (let* ((n (array-dimension m1 1))
	 (m3 (make-array `(,n ,n) :initial-element 0.0)))
    (loop for r from 0 below n do
      (loop for c from 0 below n do
	(loop for i from 0 below n do
	  (incf (aref m3 r c) (* (aref m1 r i) (aref m2 i c))))))
    m3))

(defun tr-scale (matrix x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) x
     (aref tr 1 1) y
     (aref tr 2 2) z)
    (matrix-mulf matrix tr)))

(defun tr-translate (matrix x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 3) x
     (aref tr 1 3) y
     (aref tr 2 3) z)
    (matrix-mulf matrix tr)))

(defun tr-rotate (matrix angle x y z)
  (let ((tr (make-identity-matrixf 4))
	(cosv (cos angle))
	(sinv (sin angle)))
    (setf
     (aref tr 0 0) (+ cosv (* (expt x 2) (- 1 cosv)))
     (aref tr 0 1) (- (* x y (- 1 cosv)) (* z sinv))
     (aref tr 0 2) (+ (* x z (- 1 cosv)) (* y sinv))
     (aref tr 1 0) (- (* y x (- 1 cosv)) (* z sinv))
     (aref tr 1 1) (+ cosv (* (expt y 2) (- 1 cosv)))
     (aref tr 1 2) (- (* y z (- 1 cosv)) (* x sinv))
     (aref tr 2 0) (- (* z x (- 1 cosv)) (* y sinv))
     (aref tr 2 1) (+ (* z y (- 1 cosv)) (* x sinv))
     (aref tr 2 2) (+ cosv (* (expt z 2) (- 1 cosv))))
    (matrix-mulf matrix tr)))
