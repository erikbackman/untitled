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

(defun quad-vertex-array? (array)
  (every #'quad-vertex-p array))

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

(defparameter *white* #(1.0 1.0 1.0 1.0))
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
		      :quad-count 0
		      :quad-max-count 40
		      :offs 0))
   
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
    (let ((data `#(,(make-quad-vertex :position #(-0.5 -0.5 +0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(+0.5 -0.5 +0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(-0.5 +0.5 +0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(+0.5 +0.5 +0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))

		   ,(make-quad-vertex :position #(-0.5 -0.5 -0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(+0.5 -0.5 -0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(-0.5 +0.5 -0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0))
		   ,(make-quad-vertex :position #(+0.5 +0.5 -0.5 1.0) :color #(+0.7 +0.0 +0.0 +1.0)))))
      (upload-data quad-vertex-buffer data))
    
    (set-index-buffer quad-vertex-array
		      (make-instance 'index-buffer :data #(0  1  2  2  3  1
							   4  5  6  6  7  5
							   8  9 10 10  9 11
							   12 13 14 14 13 15)))
    (check-gl-error)))



#| ================================================== |#
#| WIP                                                |#
#| ================================================== |#

(defun renderer-draw-quad (color &optional quad)
  (with-slots (quad-vertex-buffer quad-vertex-array) *renderer*
    (set-data quad-vertex-buffer :data (quad-set-color (or quad *quad-verts-f*) color))
    (let ((ib (make-instance 'index-buffer :data *quad-ix*)))
      (set-index-buffer quad-vertex-array ib))))

(defun vertex-array-size (array)
  (* 8 (array-total-size array)))

(defun upload-data (buffer vertex-array)
  "Upload VERTEX-ARRAY to a vertex BUFFER where VERTEX-ARRAY is an array of vertices of the form:
(:position #(x y z w) :color (r g b a)).

Return value: The amount of bytes written to the BUFFER."
  (let* ((total-size (vertex-array-size vertex-array))
	 (glarray (gl:alloc-gl-array :float total-size)))
    (let ((offset 0) (gl-index 0))
      (loop for vertex across vertex-array
	    ;; Write the position data to the array
	    do (loop for p across (quad-vertex-position vertex)
		     do (setf (gl:glaref glarray gl-index) p)
			;; (print (format nil "glarray[~a] = ~a" gl-index p))
			(incf gl-index))
	       ;; Followed by the color data
	       (loop for c across (quad-vertex-color vertex)
		     do (setf (gl:glaref glarray gl-index) c)
			;; (print (format nil "glarray[~a] = ~a" gl-index c))
			(incf gl-index))
	       ;; Allocated 8 floats, so increment the offset by 8 * float-size
	    do (incf offset (* 8 4)))
      (bind buffer)
      (gl:buffer-sub-data :array-buffer glarray)
      (gl:free-gl-array glarray)
      (unbind buffer)
      offset)))
