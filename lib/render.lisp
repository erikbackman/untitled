(in-package :g3d)

(defun check-gl-error ()
  (do ((err (gl:get-error) (gl:get-error)))
      ((eq :zero err))
    (print err)))

(defparameter *log* nil)
(defun log! (msg) (when *log* (print msg)))

(defparameter *origin* (sb-cga:vec 0.0 0.0 0.0))

(defparameter *white* #(1.0 1.0 1.0 1.0))
(defparameter *red* #(1.0 0.0 0.0 1.0))
(defparameter *green* #(0.0 1.0 0.0 1.0))
(defparameter *blue* #(0.0 0.0 1.0 1.0))
(defparameter *cyan* #(0.0 1.0 1.0 0.4))
(defparameter *faded* #(0.95 0.95 0.95 0.6))
(defparameter *black* #(0.0 0.0 0.0 1.0))
(defparameter *dusk-blue* #(0.30 0.42 0.47 1.0))

#|================================================================================|#
#| Renderer                                                                       |#
#|================================================================================|#

(defparameter *renderer* nil)

(defparameter *max-quads* 40)

(defstruct quad-vertex
  (position (cg:vec 0.0 0.0 0.0))
  (color #(1.0 1.0 1.0 1.0)))

(defun quad-vertex-array? (array)
  (every #'quad-vertex-p array))

(defstruct line-vertex
  (position (cg:vec 0.0 0.0 0.0))
  (color #(1.0 1.0 1.0 1.0)))

(defstruct sphere-vertex
  (position (cg:vec 0.0 0.0 0.0))
  (color #(1.0 1.0 1.0 1.0)))

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

  (line-va)
  (line-vb)
  (line-shader)
  (line-vertex-data)
  (line-count)

  (sphere-va)
  (sphere-vb)
  (sphere-ib)
  (sphere-vertex-data)
  (sphere-vertex-positions)
  (sphere-shader)
  (sphere-indices)
  (sphere-index-count)

  (max-indices)
  (draw-calls))

(defun size-of (type)
  (case type
    (:float 4)
    (:vec3 12)
    (:vec4 16)
    (:mat4 64)
    (:quad-vertex 28)
    (:line-vertex 28)))

(defun renderer-reset-stats ()
  (with-slots (draw-calls) *renderer*
    (setf draw-calls 0)))

(defun make-quad-indices (count)
  (let ((quad-indices (make-array 0 :fill-pointer 0))
	(offset 0))
    (do ((i 0 (+ 6 i)))
	((>= i count) quad-indices)
      (vector-push-extend (+ offset 0) quad-indices)
      (vector-push-extend (+ offset 1) quad-indices)
      (vector-push-extend (+ offset 2) quad-indices)
      (vector-push-extend (+ offset 2) quad-indices)
      (vector-push-extend (+ offset 3) quad-indices)
      (vector-push-extend (+ offset 0) quad-indices)
      (incf offset 4))))

(defun make-sphere-indices (n)
  (let ((indices (make-array 0 :fill-pointer 0))
	(offset 0))
    (dotimes (i n)
      (vector-push-extend (+ offset 0) indices)
      (vector-push-extend (+ offset 1) indices)
      (vector-push-extend (+ offset 2) indices)
      (incf offset 1))
    indices))

(defun draw-indexed (va count)
  (bind va)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count count))

(defun draw-lines (va count)
  (bind va)
  (gl:draw-arrays :lines 0 count))

(defun renderer-init ()
  (setf *renderer* nil)
  (let* ((max-quads 10000)
	 (max-vertices (* 4 max-quads))
	 (max-indices (* 6 max-quads))

	 ;; Quads
	 (vb (make-instance 'vertex-buffer :size (* max-vertices (size-of :quad-vertex))))
	 (ib (make-instance 'index-buffer :data (make-quad-indices max-indices)))
	 (va (make-instance 'vertex-array))
	 (shader (shader-from-file "shaders/shader.glsl"))

	 ;; Lines
	 (lvb (make-instance 'vertex-buffer :size (* max-vertices (size-of :line-vertex))))
	 (lva (make-instance 'vertex-array))
	 (lshader (shader-from-file "shaders/shader.glsl"))

	 ;; Sphere
	 (svb (make-instance 'vertex-buffer :size (* max-vertices (size-of :quad-vertex))))
	 (sib (make-instance 'index-buffer :data (make-sphere-indices max-indices)))
	 (sva (make-instance 'vertex-array))
	 (sshader (shader-from-file "shaders/shader.glsl")))

    (unbind vb)
    (unbind va)
    (unbind lvb)
    (unbind lva)
    (unbind svb)
    (unbind sva)

    ;; Quads
    (add-vertex-buffer va vb (mk-buffer-layout '(:type (:float 3) :name "a_position")
					       '(:type (:float 4) :name "a_color")))
    (set-index-buffer va ib)

    ;; Lines
    (add-vertex-buffer lva lvb (mk-buffer-layout '(:type (:float 3) :name "a_position")
						 '(:type (:float 4) :name "a_color")))

    ;; Spheres
    (add-vertex-buffer sva svb (mk-buffer-layout '(:type (:float 3) :name "a_position")
						 '(:type (:float 4) :name "a_color")))
    ;;(set-index-buffer sva sib)

    (setf *renderer* (make-renderer
		      :quad-va va
		      :quad-vb vb
		      :quad-ib ib
		      :quad-vertex-positions (vector
					      (cg:vec -0.5 -0.5 +0.5)
					      (cg:vec +0.5 -0.5 +0.5)
					      (cg:vec +0.5 +0.5 +0.5)
					      (cg:vec -0.5 +0.5 +0.5))
		      :quad-shader shader
		      :quad-count 0
		      :quad-max-count max-quads
		      :quad-vertex-data (make-array 0 :fill-pointer 0)
		      :max-indices max-indices
		      :quad-index-count 0
		      :quad-vertex-count 0

		      :sphere-va sva
		      :sphere-vb svb
		      :sphere-ib sib
		      :sphere-shader sshader
		      :sphere-vertex-data (make-array 0 :fill-pointer 0)
		      :sphere-index-count 0

		      :line-va lva
		      :line-vb lvb
		      :line-shader lshader
		      :line-vertex-data (make-array 0 :fill-pointer 0)
		      :line-count 0

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
    (shader-set-mat4 shader "u_model" (cg:translate (vec 0.0 0.0 0.0)))
    (shader-set-float shader "u_ambient" 1.0 1.0 1.0 1.0)))

(defun renderer-end-scene ()
  (renderer-flush))

(defun renderer-set-clear-color (rgba)
  (gl:clear-color (aref rgba 0) (aref rgba 1) (aref rgba 2) (aref rgba 3)))

#|================================================================================|#
#| Batching                                                                       |#
#|================================================================================|#

(defun begin-batch ()
  (with-slots (quad-index-count quad-vertex-count quad-count quad-vertex-data quad-vertex-data-base)
      *renderer*
    (setf quad-index-count 0)
    (setf quad-vertex-count 0)
    (setf quad-count 0)
    (setf (fill-pointer quad-vertex-data) 0))
  (with-slots (sphere-index-count sphere-vertex-data) *renderer*
    (setf sphere-index-count 0)
    (setf (fill-pointer sphere-vertex-data) 0))
  (with-slots (line-vertex-data line-count) *renderer*
    (setf line-count 0)
    (setf (fill-pointer line-vertex-data) 0)))

(defun next-batch ()
  (renderer-flush)
  (setf (fill-pointer (renderer-quad-vertex-data *renderer*)) 0)
  (setf (fill-pointer (renderer-line-vertex-data *renderer*)) 0)
  (setf (fill-pointer (renderer-sphere-vertex-data *renderer*)) 0)
  ;;(begin-batch)
  )

(defmacro render-batch (&body body)
  `(progn
     (begin-batch)
     ,@body
     (next-batch)))

(defun new-batch? (vertex-data)
  (plusp (fill-pointer vertex-data)))

(defun shutdown ()
  (with-slots (quad-ib quad-vb quad-va line-vb line-va sphere-ib sphere-vb sphere-va) *renderer*
    (gl:delete-buffers `(,(id quad-ib)
			 ,(id quad-vb)
			 ,(id quad-va)
			 ,(id line-va)
			 ,(id line-va)
			 ,(id sphere-ib)
			 ,(id sphere-vb)
			 ,(id sphere-va)))))

(defun upload-data (buffer vertex-array)
  (declare (ignore buffer))
  "Upload VERTEX-ARRAY to a vertex BUFFER where VERTEX-ARRAY is an array of vertices of the form:
(:position #(x y z w) :color (r g b a))."
  ;; total-size: 7 elements per vertex (3 for pos and 4 for color) *
  ;; how many elements we pushed.
  (let* ((total-size (* 7 (fill-pointer vertex-array)))
	 (glarray (gl:alloc-gl-array :float total-size))
	 (gl-index 0))
    ;; The fill-pointer gets reset to 0 by `begin-batch' for every new
    ;; batch and then moved up to the count of vertices in a batch.
    ;; Every element past the fill pointer will just be 0.
    (loop for i from 0 below (fill-pointer vertex-array)
	  for vertex = (aref vertex-array i)
	  ;; Write the position data to the array
	  do (loop for p across (slot-value vertex 'position)
		   do (setf (gl:glaref glarray gl-index) p)
		      (incf gl-index))
	     ;; Followed by the color data
	     (loop for c across (slot-value vertex 'color)
		   do (setf (gl:glaref glarray gl-index) c)
		      (incf gl-index)))
    (gl:buffer-sub-data :array-buffer glarray :buffer-offset 0)
    (gl:free-gl-array glarray)))

(defun renderer-flush ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)

  (with-slots (sphere-vb sphere-va sphere-shader sphere-vertex-data sphere-ib sphere-index-count) *renderer*
    (when (plusp sphere-index-count)
      (bind sphere-vb)
      (bind sphere-va)
      (set-index-buffer sphere-va sphere-ib)
      (when (new-batch? sphere-vertex-data)
	(upload-data sphere-vb sphere-vertex-data))
      (draw-indexed sphere-va sphere-index-count)
      (unbind sphere-va)
      (incf (renderer-draw-calls *renderer*))))
  
  (with-slots (quad-vb quad-va quad-shader quad-vertex-data quad-ib quad-index-count) *renderer*
    (shader-set-mat4 quad-shader "u_view" (camera-view *camera*))
    (shader-set-mat4 quad-shader "u_proj" (camera-projection *camera* *aspect*))
    (when (plusp quad-index-count)
      (bind quad-vb)
      (bind quad-va)
      (set-index-buffer quad-va quad-ib)
      (when (new-batch? quad-vertex-data)
	(upload-data quad-vb quad-vertex-data))
      (draw-indexed quad-va quad-index-count)
      (unbind quad-va)
      (unbind quad-ib)
      (incf (renderer-draw-calls *renderer*))))
  
  (with-slots (line-vertex-data line-vb line-va line-count) *renderer*
    (when (plusp line-count)
      (bind line-vb)
      (when (new-batch? line-vertex-data)
	(upload-data line-vb line-vertex-data))
      (draw-lines line-va (* 2 line-count))
      (incf (renderer-draw-calls *renderer*)))))

(defun quad-index-count-maxed? (renderer)
  (with-slots (quad-index-count max-indices) renderer
    (>= quad-index-count max-indices)))

#|================================================================================|#
#| Quads                                                                          |#
#|================================================================================|#

(defun draw-quad-transform (transform &optional (color *white*))
  (with-slots (quad-vertex-data quad-vertex-positions quad-index-count quad-count quad-vertex-count) *renderer*

    (when (quad-index-count-maxed? *renderer*)
      (next-batch))

    (dotimes (i 4)
      (vector-push-extend
       (make-quad-vertex
	:position (cg:transform-point (aref quad-vertex-positions i) transform)
	:color color)
       quad-vertex-data))

    (incf quad-index-count 6)
    (incf quad-count)
    (incf quad-vertex-count 4)))

(defun draw-quad-at (x y z &optional color)
  (with-slots (quad-vb quad-vertex-data) *renderer*
    (draw-quad-transform (cg:translate (cg:vec x y z)) (or color *white*))))

(defun draw-quad (&optional color) (draw-quad-at 0 0 0 color))

(defun draw-quad-rotated (x y z rotation axis &optional color (scale-x 1.0) (scale-y 1.0))
  (with-slots (quad-vertex-positions quad-vertex-data quad-index-count quad-count quad-vertex-count) *renderer*
    (let ((transform (matrix* (cg:translate (cg:vec x y z) )
			      (cg:rotate-around axis (deg->rad rotation))
			      (cg:scale* scale-x scale-y 1.0))))
      (draw-quad-transform transform color))))

#|================================================================================|#
#| Cubes                                                                          |#
#|================================================================================|#

(defun draw-cube (x y z)
  (draw-quad-at (- 0.5 x) (- 0.5 y) (- 0.5 z) *green*)
  (draw-quad-at (- 0.5 x) (- 0.5 y) (- -0.5 z) *green*)
  (draw-quad-rotated (- -0.5 x) (- 0.5 y) (- 0.5 z) 90 (cg:vec 0.0 1.0 0.0) *red*)
  (draw-quad-rotated (- +0.5 x) (- 0.5 y) (- 0.5 z) 90 (cg:vec 0.0 1.0 0.0) *red*)
  (draw-quad-rotated (- +0.5 x) (- 0.5 y) (- 0.5 z) 90 (cg:vec 1.0 0.0 0.0) *blue*)
  (draw-quad-rotated (- +0.5 x) (- 1.5 y) (- 0.5 z) 90 (cg:vec 1.0 0.0 0.0) *blue*))

#|================================================================================|#
#| Lines                                                                          |#
#|================================================================================|#

(defun draw-line (p0 p1 &optional color)
  (with-slots (line-vertex-data line-count) *renderer*
    (let ((c (or color *white*)))
      (vector-push-extend (make-line-vertex :position p0 :color c) line-vertex-data)
      (vector-push-extend (make-line-vertex :position p1 :color c) line-vertex-data))
    (incf line-count)))

#|================================================================================|#
#| Planes                                                                         |#
#|================================================================================|#

(defun plane-vertices (center normal tangent)
  (let* ((Y (sb-cga:normalize (sb-cga:cross-product normal tangent)))
	 (X (sb-cga:normalize (sb-cga:cross-product normal Y))))
    (let* ((x (sb-cga:vec* X (/ (sb-cga:vec-length X) 2)))
	   (y (sb-cga:vec* Y (/ (sb-cga:vec-length Y) 2)))
	   (v1 (sb-cga:vec- (sb-cga:vec- center x) y))  ;; o - x - y (0)
	   (v2 (sb-cga:vec+ (sb-cga:vec- center x) y))  ;; o - x + y (1)
	   (v3 (sb-cga:vec+ (sb-cga:vec+ center x) y))  ;; o + x + y (2)
	   (v4 (sb-cga:vec- (sb-cga:vec+ center x) y))  ;; o + x - y (3)
	   )
      `#(,v1 ,v2 ,v3 ,v4))))

(defun plane (normal tangent &optional center scale)
  (let ((vs (plane-vertices (or center *origin*) normal tangent))
	(r (make-array 4 :fill-pointer 0))
	(tr (cg:scale scale)))
    (do ((i 0 (+ i 1)))
	((= i 4) r)
      (vector-push
       (make-quad-vertex :position (cg:transform-point (aref vs i) tr) :color *cyan*) r))))

(defun draw-plane-normal (normal &optional center (scale (vec 25.0 25.0 25.0)))
  (with-slots (quad-vertex-data quad-index-count quad-count quad-vertex-count) *renderer*
    (let* ((vs (plane normal (sb-cga:vec 1.0 0.0 0.0) center scale)))
      (vector-push-extend (aref vs 0) quad-vertex-data)
      (vector-push-extend (aref vs 1) quad-vertex-data)
      (vector-push-extend (aref vs 2) quad-vertex-data)
      (vector-push-extend (aref vs 3) quad-vertex-data)
      (incf quad-index-count 6)
      (incf quad-count)
      (incf quad-vertex-count 4))))

(defun draw-plane-points (p1 p2 p3)
  (draw-plane-normal (cg:cross-product (cg:vec- p1 p2) (cg:vec- p1 p3))))

(defun draw-plane-expr (expr)
  (draw-plane-normal (expr->normal expr)))

(defun expr->normal (expr)
  (let ((x 0.0)
	(y 0.0)
	(z 1.0))
    (declare (type (single-float) x y z))
    (loop with op = (elt expr 0)
	  for e in (rest expr)
	  for l = (length e)
	  for k = (coerce (if (= 2 l) (elt e 0) 1) 'single-float)
	  for a = (if (= 1 l) (elt e 0) (elt e 1))
	  do (cond ((eq 'x a) (setf x k))
		   ((eq 'y a) (setf y k))
		   ((eq 'z a) (setf z k)))
	  finally (return (sb-cga:vec (/ x z) z (- (/ y z)))))))


#|================================================================================|#
#| Prisms                                                                         |#
#|================================================================================|#

;; Todo generate these for different prisms
(defun draw-prism-transform (transform)
  (let ((verts (vector
		(vec -0.5 -0.5 +0.5)
		(vec +0.5 -0.5 +0.5)
		(vec +0.5 +0.5 +0.5)
		(vec -0.5 +0.5 +0.5)

		(vec +0.0 -0.5 -0.5)
		(vec -0.5 -0.5 +0.5)
		(vec -0.5 +0.5 +0.5)
		(vec +0.0 +0.5 -0.5)

		(vec +0.0 -0.5 -0.5)
		(vec +0.5 -0.5 +0.5)
		(vec +0.5 +0.5 +0.5)
		(vec +0.0 +0.5 -0.5)

		;; Should use a triangle-vertex buffer for top and bot?
		(vec -0.5 +0.5 +0.5)
		(vec +0.0 +0.5 +0.5)
		(vec +0.0 +0.5 -0.5)
		(vec +0.5 +0.5 +0.5)

		(vec -0.5 -0.5 +0.5)
		(vec +0.0 -0.5 +0.5)
		(vec +0.0 -0.5 -0.5)
		(vec +0.5 -0.5 +0.5))))
    (with-slots (quad-vertex-data) *renderer*
      (loop for v across verts
	    do (vector-push-extend
		(make-quad-vertex :position (transform-point v transform) :color *blue*) quad-vertex-data)))

    (with-slots (quad-index-count quad-count quad-vertex-count) *renderer*
      (incf quad-index-count 18)
      (incf quad-count 3)
      (incf quad-vertex-count 6)
      ;; for top and bot
      (incf quad-index-count 3)
      (incf quad-count 2)
      (incf quad-vertex-count 3))))

(defun draw-prism-at (x y z)
  (draw-prism-transform (matrix* (translate (vec x (+ y 0.0) (- z))))))

#|================================================================================|#
#| Triangles                                                                      |#
#|================================================================================|#

(defun draw-triangle-transform (transform)
  (with-slots (quad-vertex-data quad-index-count quad-count quad-vertex-count) *renderer*
    (loop for v across (vector (vec -0.5 +0.5 +0.5)
			       (vec +0.0 +0.5 +0.5)
			       (vec +0.0 +0.5 -0.5)
			       (vec +0.5 +0.5 +0.5))
	  do (vector-push-extend
	      (make-quad-vertex :position (transform-point v transform) :color *blue*) quad-vertex-data))
    (incf quad-index-count 6)
    (incf quad-count)
    (incf quad-vertex-count 6)))

(defun draw-triangle-at (x y z)
  (draw-triangle-transform (cg:translate* (- x 0.0) (- y 0.0) (- z 0.0))))

#|================================================================================|#
#| Spheres                                                                        |#
#|================================================================================|#

(defun sphere (r p)
  (let ((verts (make-array `(,(+ p 1) ,(+ p 1)))))
    (dotimes (i (+ p 1))
      (let ((lat (linear-map i 0 p (- +pi/2+) +pi/2+)))
	(dotimes (j (+ p 1))
	  (let* ((lon (linear-map j 0 p (- +pi+) +pi+))
		 (x (* r (sin lon) (cos lat)))
		 (y (* r (sin lon) (sin lat)))
		 (z (* r (cos lon))))
	    (setf (aref verts i j) (vec x y z))
	    ))))
    verts))

(defparameter +unit-sphere-vertex-count+ 100)
(defparameter +unit-sphere+ (sphere 1 +unit-sphere-vertex-count+))

(defun draw-sphere (radius)
  (let* ((vertex-count +unit-sphere-vertex-count+)
	 (globe (sphere 1.0 vertex-count))
	 (trans (cg:scale* radius radius radius)))
    (with-slots (sphere-vertex-data sphere-index-count) *renderer*
      (dotimes (i vertex-count)
	(dotimes (j (+ vertex-count 1))
	  (let ((v1 (aref globe i j))
		(v2 (aref globe (+ 1 i) j)))
	    (vector-push-extend
	     (make-quad-vertex :position (cg:transform-point v1 trans) :color *red*) sphere-vertex-data)
	    (vector-push-extend
	     (make-quad-vertex :position (cg:transform-point v2 trans) :color *red*) sphere-vertex-data))))
      (incf sphere-index-count (* 6 (array-total-size globe))))))

