(in-package :untitled)

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count (slot-value ib 'count)))

#|================================================================================|# 
#| Renderer                                                                       |# 
#|================================================================================|# 

;; Work in progress, see demo.lisp for a working example.

(defparameter *renderer* nil)

(defstruct quad-vertex
  (position #(0 0 0) :type (simple-vector 3))
  (color #(1.0 1.0 1.0 1.0) :type (simple-vector 4)))

(defstruct renderer
  (quad-vertex-array)
  (quad-vertex-buffer)
  (quad-vertex-positions)
  (quad-shader))

(defparameter *quad-verts* #(+0.5 +0.5 +0.0    +0.0 +0.8 +0.8 +0.3 ; 0
			     +0.5 -0.5 +0.0    +0.0 +0.8 +0.8 +0.3 ; 1
			     -0.5 -0.5 +0.0    +0.0 +0.8 +0.8 +0.3 ; 2 
			     -0.5 +0.5 +0.0    +0.0 +0.8 +0.8 +0.3 ; 3
			     ))

(defparameter *red* #(0.0 0.0 1.0 1.0))
(defparameter *green* #(0.0 1.0 0.0 1.0))
(defparameter *blue* #(0.0 0.0 1.0 1.0))

(defun renderer-init ()
  (setf *renderer* (make-renderer
		    :quad-vertex-array (make-instance 'vertex-array)
		    :quad-vertex-buffer (make-instance 'vertex-buffer :data *quad-verts*)
		    :quad-vertex-positions #()
		    :quad-shader (with-slots (vs fs) (load-shader "shader.glsl")
				   (create-shader vs fs))))
  
  (add-vertex-buffer (renderer-quad-vertex-array *renderer*)
		     (renderer-quad-vertex-buffer *renderer*)
		     (mk-buffer-layout '(:type (:float 3) :name "a_position")
				       '(:type (:float 4) :name "a_color")))
  (let ((quad-ib (make-instance 'index-buffer :data #(0 1 2 2 3 0))))
    (set-index-buffer (renderer-quad-vertex-array *renderer*) quad-ib)))

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

(defun quad-set-color (arr color)
  (let ((v (alexandria:copy-array arr))) 
    (loop for r from 0 below 4 do
      (loop for c from 3 below 7
	    for i from 0 to 3 do
	      (setf (aref v (+ (* r 7) c)) (aref color i))))
    v))

;; draw-quad (transform color)
(defun renderer-draw-quad (position color)
  (with-slots (quad-vertex-buffer) *renderer*
    (set-data quad-vertex-buffer (quad-set-color position color))))
