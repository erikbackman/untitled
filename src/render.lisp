(in-package :untitled)

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count (slot-value ib 'count)))

(defun check-gl-error ()
  (let ((err (gl:get-error)))
    (loop while (not (eq :zero err))
	  do (print err)
	     (setf err (gl:get-error)))))

#|================================================================================|# 
#| Renderer                                                                       |# 
#|================================================================================|#

;; Work in progress, see demo.lisp for a working example.

(defparameter *renderer* nil)

(defstruct quad-vertex
  (position #(0.0 0.0 0.0 1.0) :type (simple-vector 4))
  (color #(1.0 1.0 1.0 1.0) :type (simple-vector 4)))

(defstruct renderer
  (quad-vertex-array)
  (quad-vertex-buffer)
  (quad-vertex-positions)
  (quad-vertex-base)
  (quad-shader)
  (offs))


(defparameter *quad-ix* #(0 1 2 2 3 0))

(defparameter *red* #(1.0 0.0 0.0 1.0))
(defparameter *green* #(0.0 1.0 0.0 1.0))
(defparameter *blue* #(0.0 0.0 1.0 1.0))

;; Some vertices for testing 
(defparameter *quad-verts-f* #2A((-0.5 -0.5 +0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (+0.5 -0.5 +0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (-0.5 +0.5 +0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (+0.5 +0.5 +0.5 1.0    +0.7 +0.0 +0.0 +1.0)))

(defparameter *quad-verts-b* #2A((-0.5 -0.5 -0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (+0.5 -0.5 -0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (-0.5 +0.5 -0.5 1.0    +0.7 +0.0 +0.0 +1.0)
				 (+0.5 +0.5 -0.5 1.0    +0.7 +0.0 +0.0 +1.0)))

(defun renderer-init ()
  (let ((vb (make-instance 'vertex-buffer
			   :data *quad-verts-f*
			   :size (* 4 8 4 4) ;; rows * cols * float-size * quad-count
			   ))
	
	(ib (make-instance 'index-buffer :data #()))
	(va (make-instance 'vertex-array))
	(shader (with-slots (vs fs) (load-shader "quad-shader.glsl")
		  (create-shader vs fs))))

    (set-index-buffer va ib)
    (setf *renderer* (make-renderer
		      :quad-vertex-array va
		      :quad-vertex-buffer vb
		      :quad-vertex-positions #((+0.5 +0.5 +0.0 1.0)
					       (+0.5 -0.5 +0.0 1.0)
					       (-0.5 -0.5 +0.0 1.0)
					       (-0.5 +0.5 +0.0 1.0))
		      :quad-vertex-base (make-array 6 :element-type 'quad-vertex
						      :initial-element (make-quad-vertex))
		      :quad-shader shader
		      :offs (* 4 8 4)))
   
    (add-vertex-buffer va vb
		       (mk-buffer-layout '(:type (:float 4) :name "a_position")
					 '(:type (:float 4) :name "a_color")))))

(defun renderer-begin-scene ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program (renderer-quad-shader *renderer*))
  (let* ((shader (renderer-quad-shader *renderer*))
	 (fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (camera-view *camera*)))
    (shader-set-mat4 shader "u_view" view)
    (shader-set-mat4 shader "u_proj" projection)
    (shader-set-mat4 shader "u_model" (mat4-translate 0.0 0.0 0.0))))

(defun renderer-flush ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (with-slots (quad-vertex-buffer quad-vertex-array quad-shader) *renderer*
    (let ((ib (get-index-buffer quad-vertex-array)))
      (shader-set-mat4 quad-shader "u_view" (camera-view *camera*))
      (draw-triangles ib))))


;; For testing
(defun render-basic-scene ()
  (with-slots (quad-vertex-buffer quad-vertex-array offs) *renderer*
    ;; offset 128 as in 4(rows) * 8(cols) * 4(float-size)
    (set-data quad-vertex-buffer :data *quad-verts-b* :offset 128)
    (set-index-buffer quad-vertex-array
		      (make-instance 'index-buffer :data #(0 1 2 2 3 1
							   4 5 6 6 7 5)))
    (check-gl-error)))



#| ================================================== |#
#| WIP                                                |#
#| ================================================== |#


(defun quad-set-color (arr color)
  (let ((arr2 (alexandria:copy-array arr :adjustable t)))
    (loop for r from 0 below 4 do
      (loop for c from 4 below 8
	    for i from 0 to 4 do
	      (setf (aref arr2 r c) (aref color i))))
    arr2))

(defun quad-set-color2 (color arr)
  (let ((arr2 (make-array '(4 8))))
    (loop for i from 0 below 4 do
	  (loop for j from 0 below 8 do
		(if (>= j 4) (setf (aref arr2 i j) (aref color (- j 4)))
		    (setf (aref arr2 i j) (aref arr i j)))))
    
    arr2))


(defun renderer-draw-quad (color &optional quad)
  (with-slots (quad-vertex-buffer quad-vertex-array) *renderer*
    (set-data quad-vertex-buffer :data (quad-set-color (or quad *quad-verts-f*) color))
    (let ((ib (make-instance 'index-buffer :data *quad-ix*)))
      (set-index-buffer quad-vertex-array ib))))
