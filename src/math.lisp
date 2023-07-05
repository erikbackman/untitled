(in-package :untitled)

(defconstant +pi/6+ (/ pi 6))
(defconstant +pi/4+ (/ pi 4))
(defconstant +pi/3+ (/ pi 3))
(defconstant +pi/2+ (/ pi 2))
(defconstant +2pi+ (* 2 pi))

(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun make-identity-matrixf (n)
  (let ((m (make-array (list n n) :initial-element 0.0)))
    (dotimes (i n)
      (setf (aref m i i) 1.0))
    m))

(defparameter +identity-matrix4+ 
  #2A((1.0 0.0 0.0 0.0)
      (0.0 1.0 0.0 0.0)
      (0.0 0.0 1.0 0.0)
      (0.0 0.0 0.0 1.0)))

(defun make-identity-matrixf-4f ()
  (alexandria:copy-array +identity-matrix4+))

(defun cotan (a)
  (/ 1 (tan a)))

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

(defmacro matrix* (&rest mats)
  (reduce (lambda (l r) `(matrix-mulf ,l ,r)) mats :from-end t))

(defmacro def-transformation (name args &body matrix-form)
  (let ((mat4f (read-from-string (format nil "tr-mat4-~a" name)))
	(mat4mulf (read-from-string (format nil "tr-~a" name))))
    `(progn (defun ,mat4f ,args ,@matrix-form)
	    (defun ,mat4mulf (,@args matrix) 
	      (matrix-mulf (,mat4f ,@args) matrix)))))

(defun tr-mat4-scale (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) x
     (aref tr 1 1) y
     (aref tr 2 2) z)
    tr))

;; | x*x*(1-c)+c      x*y*(1-c)-z*s    x*z*(1-c)+y*s |
;; | y*x*(1-c)+z*s    y*y*(1-c)+c      y*z*(1-c)-x*s |
;; | z*x*(1-c)-y*s    z*y*(1-c)+x*s    z*z*(1-c)+c   |
(defun tr-mat4-rotate (angle x y z)
  (let* ((tr (make-identity-matrixf 4))
	 (c (cos angle))
	 (s (sin angle))
	 (1-c (- 1 c)))
    (setf
     (aref tr 0 0) (+ (* x x 1-c) c)
     (aref tr 0 1) (- (* x y 1-c) (* z s))
     (aref tr 0 2) (+ (* x z 1-c) (* y s))
     
     (aref tr 1 0) (+ (* y x 1-c) (* z s))
     (aref tr 1 1) (+ (* y y 1-c) c)
     (aref tr 1 2) (- (* y z 1-c) (* x s))
     
     (aref tr 2 0) (- (* z x 1-c) (* y s))
     (aref tr 2 1) (+ (* z y 1-c) (* x s))
     (aref tr 2 2) (+ (* z z 1-c) c))
    tr))

(defun tr-mat4-translate (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 3) (- x)
     (aref tr 1 3) (- y)
     (aref tr 2 3) (- z))
    tr))

(defun tr-mat4-ortho (right left bottom top znear zfar)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) (/ 2 (- right left))
     (aref tr 1 1) (/ 2 (- top bottom))
     (aref tr 2 2) (/ -2 (- zfar znear))
     (aref tr 0 3) (- (/ (+ right left) (- right left)))
     (aref tr 1 3) (- (/ (+ top bottom) (- top bottom)))
     (aref tr 2 3) (- (/ (+ zfar znear) (- zfar znear))))
    tr))

(defun tr-mat4-perspective (fovy aspect znear zfar)
  (let ((tr (make-identity-matrixf 4))
	(f (cotan (/ fovy 2))))
    (setf
     (aref tr 0 0) (/ 1 (* f aspect))
     (aref tr 1 1) (/ 1 f)
     (aref tr 2 2) (- (/ (+ zfar znear) (- zfar znear)))
     (aref tr 2 3) -1.0
     (aref tr 3 2) (- (/ (* 2 zfar znear) (- znear zfar))))
    tr))
