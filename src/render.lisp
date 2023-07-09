(in-package :untitled)

(defgeneric bind (obj))
(defgeneric unbind (obj))

(defun alloc-gl-array (data size target)
  (let ((arr (gl:alloc-gl-array :float size)))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:buffer-data target :static-draw arr)
    (gl:free-gl-array arr)))

;; ========  Vertex buffer ========

(defclass vertex-buffer ()
  ((id :accessor id)))

(defmethod initialize-instance :after ((obj vertex-buffer) &key data (size (length data)))
  (with-slots (id) obj
    (setf id (gl:gen-buffer))
    (gl:bind-buffer :array-buffer id)
    (alloc-gl-array data size :array-buffer)))

(defmethod bind ((obj vertex-buffer))
  (with-slots (id) obj (gl:bind-buffer :array-buffer id)))

(defmethod unbind ((obj vertex-buffer))
  (gl:bind-buffer :array-buffer 0))

;; ========  Index buffer ========

(defclass index-buffer ()
  ((id :accessor id)
   (count :accessor index-count :initform 0)))

(defmethod initialize-instance :after ((obj index-buffer) &key data (count (length data)))
  (with-slots (id (cnt count)) obj
    (setf id (gl:gen-buffer)
	  cnt count)
    (gl:bind-buffer :element-array-buffer id)
    (let ((arr (gl:alloc-gl-array :unsigned-int count)))
      (dotimes (i count)
	(setf (gl:glaref arr i) (aref data i))
	(gl:buffer-data :element-array-buffer :static-draw arr)))))

(defun make-index-buffer (data &optional (count (length data)))
  (let ((arr (gl:alloc-gl-array :uint count)))
    (dotimes (i count)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defmethod bind ((obj index-buffer))
  (with-slots (id) obj (gl:bind-buffer :element-array-buffer id)))

(defmethod unbind ((obj index-buffer))
  (gl:bind-buffer :element-array-buffer 0))

;; ======== Vertex layout ========

(defstruct vertex-buffer-element type count normalized)

(defstruct vertex-buffer-layout
  (elements (make-array '(0) :fill-pointer 0 :adjustable t))
  (stride 0))

(defun vertex-buffer-layout-push (layout count)
  (with-slots (elements stride) layout
    (vector-push-extend (make-vertex-buffer-element :type :float :count count :normalized nil) elements)
    (setf stride (* 6 (cffi:foreign-type-size :float)))))

;; ========  Vertex array ========

(defclass vertex-array ()
  ((id :initarg :name
       :accessor id
       :initform (gl:gen-vertex-array))
   (elements :initarg :elements :accessor elements :initform nil)))

(defmethod bind ((obj vertex-array))
  (with-slots (id) obj (gl:bind-vertex-array id)))

(defmethod unbind ((obj vertex-array))
  (gl:bind-vertex-array 0))

(defun vertex-array-add-buffer (va vb layout)
  (bind va)
  (bind vb)
  (with-slots (elements stride) layout
    (loop for i from 0 below (length elements)
	  for elem = (aref elements i)
	  do
	     (gl:enable-vertex-attrib-array i)
	     (gl:vertex-attrib-pointer i 3 :float nil stride (cffi:null-pointer))

	     (gl:enable-vertex-attrib-array (+ 1 i))
	     (gl:vertex-attrib-pointer (+ 1 i) 3 :float nil stride
				       (cffi-sys:inc-pointer
					(cffi:null-pointer)
					(* (slot-value elem 'count)
					   (cffi:foreign-type-size :float)))))))
;; ======== Renderer ========

(defparameter *cube-positions*
  #(
    #( 2.0  5.0 -15.0)
    #(-1.5 -2.2 -2.5)
    #(-3.8 -2.0 -12.3)
    #( 2.4 -0.4 -3.5)
    #(-1.7  3.0 -7.5)
    #( 1.3 -2.0 -2.5)
    #( 1.5  2.0 -2.5)
    #( 1.5  0.2 -1.5)
    #(-1.3  1.0 -1.5)
    ))

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count (slot-value ib 'count)))


(defun draw (va ib shader)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program shader)
  (let* ((fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (camera-view *camera*)))
        
    (shader-set-mat4 shader "u_view" view)
    (shader-set-mat4 shader "u_proj" projection)

    (bind va)
    (bind ib)
    
    (let ((time (* 2 (glfw:get-time))))
      (loop for pos across *cube-positions*
	    for i by 1
	    for angle = (* time i)
	    do
	       (shader-set-mat4 shader "u_model"
				(matrix* (mat4-rotate (deg->rad angle) 1.0 0.0 0.5)
					 (mat4-translate (vec-x pos) (vec-y pos) (vec-z pos))))

	       (draw-triangles ib))
                 
      (let ((ib (make-instance 'index-buffer :data *prism-ix*)))
	(shader-set-mat4 shader "u_model"
			 (matrix* (mat4-rotate time 0.0 1.0 0.0)
				  (mat4-translate 0 0 0)))
	(draw-triangles ib)))))
