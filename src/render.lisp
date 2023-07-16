(in-package :untitled)

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count (slot-value ib 'count)))

(defun draw-indexed (va)
  (bind va)
  (let ((count (index-count (get-index-buffer va))))
    (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		      :count count)))

(defun check-gl-error ()
  (let ((err (gl:get-error)))
    (loop while (not (eq :zero err))
	  do (print err)
	     (setf err (gl:get-error)))))

(defparameter *log* nil)
(defun log! (msg)
  (when *log* (print msg)))

#|================================================================================|# 
#| Renderer                                                                       |# 
#|================================================================================|#

;; Work in progress, see demo.lisp for a working example.

(defparameter *renderer* nil)
(defparameter *data-ready* nil)

(defparameter *max-quads* 40)
(defparameter *batch-size* 0)

(defstruct quad-vertex
  (position (sb-cga:vec 0.0 0.0 0.0))
  (color #(1.0 1.0 1.0 1.0)))

(defun quad-vertex-array? (array)
  (every #'quad-vertex-p array))

(defstruct renderer
  (quad-va)
  (quad-vb)
  (quad-ib)
  
  (quad-vertex-data)
  (quad-vertex-positions)
  (quad-shader)
  
  (quad-count)
  (quad-max-count)
  (quad-indices)
  (quad-index-count)
  (quad-vertex-count)
  
  (max-indices)
  (draw-calls))

(defparameter *white* #(1.0 1.0 1.0 1.0))
(defparameter *red* #(1.0 0.0 0.0 1.0))
(defparameter *green* #(0.0 1.0 0.0 1.0))
(defparameter *blue* #(0.0 0.0 1.0 1.0))

(defparameter *quad-vertex-default-position*
  (vector (sb-cga:vec -0.5 -0.5 +0.5)
	  (sb-cga:vec +0.5 -0.5 +0.5)
	  (sb-cga:vec -0.5 +0.5 +0.5)
	  (sb-cga:vec +0.5 +0.5 +0.5)))

(defun size-of (type)
  (case type
    (:float 4)
    (:vec3 12)
    (:vec4 16)
    (:mat4 64)))

(defun calculate-offset (quad-count)
  (if (= quad-count *max-quads*) 0
      (* 4 8 (size-of :float) quad-count)))

(defun renderer-reset-stats ()
  (with-slots (draw-calls) *renderer*
    (setf draw-calls 0)))

(defun make-quad-indices (count)
  (let* ((quad-indices (make-array count))
	 (offset 0))
    (loop for i from 0 below count by 6
	  do
	     (setf (aref quad-indices (+ i 0)) (+ offset 0))
	     (setf (aref quad-indices (+ i 1)) (+ offset 1))
	     (setf (aref quad-indices (+ i 2)) (+ offset 3))
	     (setf (aref quad-indices (+ i 3)) (+ offset 1))
	     (setf (aref quad-indices (+ i 4)) (+ offset 2))
	     (setf (aref quad-indices (+ i 5)) (+ offset 3))
	       
	     (incf offset 4))
    quad-indices))

(defun renderer-init ()
  (setf *renderer* nil)
  (let* ((max-quads 200)
	 (max-indices (* 6 max-quads))
	 (vb (make-instance 'vertex-buffer
			    :data #()
			    :size (* 2 (size-of :mat4) *max-quads*)))
	
	 (ib (make-instance 'index-buffer :data (make-quad-indices max-indices)))
	 (va (make-instance 'vertex-array))
	 (shader (with-slots (vs fs) (load-shader "shader.glsl")
		   (create-shader vs fs))))

    (add-vertex-buffer
     va vb (mk-buffer-layout '(:type (:float 3) :name "a_position")
			     '(:type (:float 4) :name "a_color")))

    (set-index-buffer va ib)
    
    (setf *renderer* (make-renderer
		      :quad-va va
		      :quad-vb vb
		      :quad-ib ib
		      :quad-vertex-positions (vector (sb-cga:vec +0.5 +0.5 +0.5)
						     (sb-cga:vec +0.5 -0.5 +0.5)
						     (sb-cga:vec -0.5 -0.5 +0.5)
						     (sb-cga:vec -0.5 +0.5 +0.5))
		      :quad-shader shader
		      :quad-count 0
		      :quad-max-count max-quads
		      :quad-vertex-data (make-array 0 :fill-pointer 0)
		      :max-indices max-indices
		      :quad-index-count 0
		      :quad-vertex-count 0
		      :draw-calls 0))))

(defun renderer-begin-scene ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program (renderer-quad-shader *renderer*))
  (let* ((shader (renderer-quad-shader *renderer*))
	 (fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (camera-view *camera*)))
    (shader-set-mat4 shader "u_view" view)
    (shader-set-mat4 shader "u_proj" projection)
    (shader-set-mat4 shader "u_model" (sb-cga:translate (vec 0.0 0.0 0.0)))))

(defun renderer-end-scene ()
  (renderer-flush))

(defun begin-batch ()
  (with-slots (quad-index-count quad-vertex-data quad-vertex-data-base) *renderer*
    (setf quad-index-count 0)
    (setf (fill-pointer quad-vertex-data) 0)))

(defun next-batch ()
  (setf *batch-size* 0)
  (renderer-flush)
  (begin-batch))

(defmacro render-batch (&body body)
  `(progn
     (begin-batch)
     ,@body
     (next-batch)))

(defun shutdown ()
  (with-slots (quad-ib quad-vb quad-va) *renderer*
    (gl:delete-buffers `(,(id quad-ib)
			 ,(id quad-vb)
			 ,(id quad-va)))))

(defun upload-data (buffer vertex-array)
  "Upload VERTEX-ARRAY to a vertex BUFFER where VERTEX-ARRAY is an array of vertices of the form:
(:position #(x y z w) :color (r g b a)).

Return value: The amount of bytes written to the BUFFER."
  (let* ((total-size (vertex-array-size vertex-array))
	 (glarray (gl:alloc-gl-array :float total-size))
	 (gl-index 0))
    ;; The fill-pointer gets reset to 0 by `begin-batch' for every new
    ;; batch and then moved up to the count of vertices in a batch.
    ;; Every element past the fill pointer will just be 0.
    (loop for i from 0 below (fill-pointer vertex-array)
	  for vertex = (aref vertex-array i)
	  ;; Write the position data to the array
	  do (loop for p across (quad-vertex-position vertex)
		   do (setf (gl:glaref glarray gl-index) p)
		      (incf gl-index))
	     ;; Followed by the color data
	     (loop for c across (quad-vertex-color vertex)
		   do (setf (gl:glaref glarray gl-index) c)
		      (incf gl-index)))
    (bind buffer)
    (gl:buffer-sub-data :array-buffer glarray :buffer-offset 0)
    (gl:free-gl-array glarray)
    (unbind buffer)))

(defun renderer-flush ()
  (with-slots (quad-vb quad-va quad-shader quad-vertex-data quad-ib) *renderer*
    (shader-set-mat4 quad-shader "u_view" (camera-view *camera*))

    (with-slots (quad-index-count) *renderer*
      (upload-data quad-vb quad-vertex-data))
    (bind quad-vb)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (bind quad-ib)
    (draw-triangles quad-ib)
    (incf (renderer-draw-calls *renderer*))))

(defun vertex-array-size (array)
  (* 8 (array-total-size array)))

(defun draw-quad (&optional color)
  (draw-quad-at 0 0 0 color))

(defun draw-quad-at (x y z &optional color)
  (assert (typep x 'single-float))
  (with-slots (quad-vb quad-vertex-data) *renderer*
    (draw-quad-transform (sb-cga:translate (sb-cga:vec x y z)) (or color *white*))))

(defun draw-quad-transform (transform color)
  (with-slots (quad-vertex-data quad-vertex-positions quad-index-count quad-count quad-vertex-count) *renderer*
    
    (when (>= (renderer-quad-index-count *renderer*) (renderer-max-indices *renderer*))
      (next-batch))

    (let ((vertex-count 4))
      (loop for i from 0 below vertex-count do
	(vector-push-extend
	 (make-quad-vertex :position (sb-cga:transform-point (aref quad-vertex-positions i) transform)
			   :color color)
	 quad-vertex-data))

      (incf *batch-size* vertex-count))
    (incf quad-index-count 6)
    (incf quad-count)
    (incf quad-vertex-count)))
