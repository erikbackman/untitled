(in-package :untitled)

(defconstant +pi/6+ (/ pi 6))
(defconstant +pi/4+ (/ pi 4))
(defconstant +pi/3+ (/ pi 3))
(defconstant +pi/2+ (/ pi 2))

(defun make-identity-matrixf (n)
  (let ((m (make-array (list n n) :initial-element 0.0)))
    (dotimes (i n)
      (setf (aref m i i) 1.0))
    m))

(defconstant +identity-matrix4+ (make-identity-matrixf 4))

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

(defun tr-mat4-scale (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) x
     (aref tr 1 1) y
     (aref tr 2 2) z)
    tr))

;;           | x*x*(1-c)+c      x*y*(1-c)-z*s    x*z*(1-c)+y*s |
;; Rotate  = | y*x*(1-c)+z*s    y*y*(1-c)+c      y*z*(1-c)-x*s |
;;           | z*x*(1-c)-y*s    z*y*(1-c)+x*s    z*z*(1-c)+c   |
(defun tr-mat4-rotate (angle x y z)
  (let ((tr (make-identity-matrixf 4))
	(c (cos angle))
	(s (sin angle)))
    (setf
     (aref tr 0 0) (+ (* x x (- 1 c)) c)
     (aref tr 0 1) (- (* x y (- 1 c)) (* z s))
     (aref tr 0 2) (+ (* x z (- 1 c)) (* y s))
     (aref tr 1 0) (+ (* y x (- 1 c)) (* z s))
     (aref tr 1 1) (+ (* y y (- 1 c)) c)
     (aref tr 1 2) (- (* y z (- 1 c)) (* x s))
     (aref tr 2 0) (- (* z x (- 1 c)) (* y s))
     (aref tr 2 1) (+ (* z y (- 1 c)) (* x s))
     (aref tr 2 2) (+ (* z z (- 1 c)) c))
    tr))

(defun tr-mat4-translate (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 3) x
     (aref tr 1 3) y
     (aref tr 2 3) z)
    tr))

(defun tr-scale (x y z matrix)
  (matrix-mulf (tr-mat4-scale x y z) matrix))

(defun tr-translate (x y z matrix)
  (matrix-mulf matrix (tr-mat4-translate x y z)))

(defun tr-rotate (angle x y z matrix)
  (matrix-mulf matrix (tr-mat4-rotate angle x y z)))

(defun tr-transform (matrix transformations)
  (reduce 'funcall transformations :from-end t :initial-value matrix))

