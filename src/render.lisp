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
  (quad-count)
  (quad-max-count)
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

(defparameter *quad-verts-t* #2A((-0.5 +0.5 +0.5 1.0    +0.0 +0.7 +0.0 +1.0) 
				 (+0.5 +0.5 +0.5 1.0    +0.0 +0.7 +0.0 +1.0)
				 (-0.5 +0.5 -0.5 1.0    +0.0 +0.7 +0.0 +1.0)
				 (+0.5 +0.5 -0.5 1.0    +0.0 +0.7 +0.0 +1.0)))

(defparameter *quad-verts-bot* #2A((-0.5 -0.5 +0.5 1.0   +0.0 +0.7 +0.0 +1.0)
				   (+0.5 -0.5 +0.5 1.0   +0.0 +0.7 +0.0 +1.0)
				   (-0.5 -0.5 -0.5 1.0   +0.0 +0.7 +0.0 +1.0)
				   (+0.5 -0.5 -0.5 1.0   +0.0 +0.7 +0.0 +1.0)))

(defparameter *max-quads* 40)

(defun size-of (type)
  (case type
    (:float 4)
    (:vec3 12)
    (:vec4 16)
    (:mat4 64)))

(defun calculate-offset (quad-count)
  (if (= quad-count *max-quads*) 0
      (* 4 8 (size-of :float) quad-count)))

(defun renderer-init ()
  (let ((vb (make-instance 'vertex-buffer
			   :data #()
			   :size (* 2 (size-of :mat4) *max-quads*)))
	
	(ib (make-instance 'index-buffer :data #()))
	(va (make-instance 'vertex-array))
	(shader (with-slots (vs fs) (load-shader "quad-shader.glsl")
		  (create-shader vs fs))))

    (set-index-buffer va ib)
    (setf *renderer* (make-renderer
		      :quad-vertex-array va
		      :quad-vertex-buffer vb
		      :quad-vertex-positions `#(,(vec4 +0.5 +0.5 +0.0 1.0)
						,(vec4 +0.5 -0.5 +0.0 1.0)
						,(vec4 -0.5 -0.5 +0.0 1.0)
						,(vec4 -0.5 +0.5 +0.0 1.0))
		      :quad-vertex-base (make-array 6 :element-type 'quad-vertex
						      :initial-element (make-quad-vertex))
		      :quad-shader shader
		      :quad-count 3
		      :quad-max-count 40
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
  (with-slots (quad-vertex-buffer quad-vertex-array quad-count) *renderer*
    ;; offset: nth-quad(0..n) * 4(rows) * 8(cols) * 4(float-size)
    (set-data quad-vertex-buffer :data *quad-verts-f*)
    (set-data quad-vertex-buffer :data *quad-verts-b* :offset 128)
    (set-data quad-vertex-buffer :data *quad-verts-t* :offset 256)
    (set-data quad-vertex-buffer :data *quad-verts-bot* :offset 384)

    (set-index-buffer quad-vertex-array
		      (make-instance 'index-buffer :data #( 0  1  2  2  3  1
							    4  5  6  6  7  5
							    8  9 10 10  9 11
							   12 13 14 14 13 15)))
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

;; TODO
;; Use 'base quad vertex positions' and translate every vertex `i' according to x y z
;; and assign it to the `i'th vector in renderers current quad.
(defun renderer-draw-quad-at (x y z &optional color)
  (with-slots (quad-vertex-buffer quad-vertex-array quad-vertex-positions quad-count) *renderer*
    (set-data quad-vertex-buffer
	      :data (quad-set-color2
		     color
		     (loop for i from 0 to 3
			   collect (matrix*v (mat4-translate x y z)
					     (aref quad-vertex-positions i))))
	      :offset (offset quad-count))))


