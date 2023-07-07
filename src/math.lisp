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

(defparameter +zero-vector+ #(0.0 0.0 0.0))

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

(defun matrix* (&rest mats)
  (reduce #'matrix-mulf mats :from-end t))

(defmacro def-transformation (name args &body matrix-form)
  (let ((mat4f (read-from-string (format nil "tr-mat4-~a" name)))
	(mat4mulf (read-from-string (format nil "tr-~a" name))))
    `(progn (defun ,mat4f ,args ,@matrix-form)
	    (defun ,mat4mulf (,@args matrix) 
	      (matrix-mulf (,mat4f ,@args) matrix)))))

(defun mat4-scale (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) x
     (aref tr 1 1) y
     (aref tr 2 2) z)
    tr))

;; | x*x*(1-c)+c      x*y*(1-c)-z*s    x*z*(1-c)+y*s |
;; | y*x*(1-c)+z*s    y*y*(1-c)+c      y*z*(1-c)-x*s |
;; | z*x*(1-c)-y*s    z*y*(1-c)+x*s    z*z*(1-c)+c   |
(defun mat4-rotate (angle x y z)
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

(defun tr-rotate (matrix angle x y z)
  (matrix-mulf (mat4-rotate angle x y z) matrix))

(defun mat4-translate (x y z)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 3) (- x)
     (aref tr 1 3) (- y)
     (aref tr 2 3) (- z)
     (aref tr 3 3) (+ 1))
    tr))

(defun mat4-ortho (right left bottom top znear zfar)
  (let ((tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) (/ 2 (- right left))
     (aref tr 1 1) (/ 2 (- top bottom))
     (aref tr 2 2) (/ -2 (- zfar znear))
     (aref tr 0 3) (- (/ (+ right left) (- right left)))
     (aref tr 1 3) (- (/ (+ top bottom) (- top bottom)))
     (aref tr 2 3) (- (/ (+ zfar znear) (- zfar znear))))
    tr))

(defun mat4-perspective (angle ratio near far)
  (let ((tr (make-identity-matrixf 4))
	(tan-half-angle (tan (/ angle 2))))
    (setf
     (aref tr 0 0) (/ 1 (* ratio tan-half-angle))
     (aref tr 1 1) (/ 1 tan-half-angle)
     (aref tr 2 2) (/ (- (+ far near) ) (- far near))
     (aref tr 3 2) (- 1)
     (aref tr 2 3) (/ (- (* 2 far near)) (- far near)))
    tr))

(defun vec- (u v)
  (let* ((dim (array-dimension u 0))
	 (w (make-array dim)))
    (dotimes (i dim)
      (setf (aref w i) (- (aref u i) (aref v i))))
    w))

(defun vec+ (u v)
  (let ((dim (array-dimension u 0)))
    (let ((w (make-array dim)))
      (dotimes (i dim)
	(setf (aref w i) (+ (aref u i) (aref v i))))
      w)))

(defun vec-= (u v)
  (dotimes (i 3)
    (setf (aref u i) (- (aref u i) (aref v i)))))

(defun vec+= (u v)
  (dotimes (i 3)
    (setf (aref u i) (+ (aref u i) (aref v i)))))

(defun vec3* (n v)
  (let ((w (make-array 3)))
    (dotimes (i 3)
      (setf (aref w i) (* n (aref v i))))
    w))

(defun vec3-incm (v &optional delta)
  (dotimes (i 3)
    (setf (aref v i) (+ (aref v i) (or delta 1)))))

(defun vec-normalize (v)
  (let* ((dim (array-dimension v 0))
	 (mag (loop for i from 0 below dim
		    for x = (aref v i)
		    sum (* x x) into total
		    finally (return (sqrt total)))))
    (if (zerop mag) +zero-vector+ ;; TODO: dirty hack for now
	(let ((w (make-array dim)))
	  (dotimes (i dim)
	    (setf (aref w i) (/ (aref v i) mag)))
	  w))))

(defun vec-cross (u v)
  (let ((w (make-array 3)))
    (setf (aref w 0)
	  (- (* (aref u 1) (aref v 2))
	     (* (aref u 2) (aref v 1)))
	  (aref w 1)
	  (- (- (* (aref u 0) (aref v 2))
		(* (aref u 2) (aref v 0))))
	  (aref w 2)
	  (- (* (aref u 0) (aref v 1))
	     (* (aref u 1) (aref v 0))))
    w))

(defun tr-look-at (eye center up)
  (let* ((f (vec-normalize (vec- eye center)))
	 (l (vec-normalize (vec-cross up f)))
	 (u (vec-cross f l))
	 (tr (make-identity-matrixf 4)))
    (setf
     (aref tr 0 0) (vec-x l)
     (aref tr 0 1) (vec-y l)
     (aref tr 0 2) (vec-z l)
     (aref tr 1 0) (vec-x u)
     (aref tr 1 1) (vec-y u)
     (aref tr 1 2) (vec-z u)
     (aref tr 2 0) (vec-x f)
     (aref tr 2 1) (vec-y f)
     (aref tr 2 2) (vec-z f)
     (aref tr 0 3) (- (dot l eye))
     (aref tr 1 3) (- (dot u eye))
     (aref tr 2 3) (- (dot f eye))
     (aref tr 3 3) 1)
    tr))
