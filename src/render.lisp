(in-package :untitled)

(defclass vx-buffer () ((id :accessor id)))

(defgeneric buffer-bind (obj))
(defgeneric buffer-unbind (obj))

(defun alloc-gl-array (data size target)
  (let ((arr (gl:alloc-gl-array :float size)))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:buffer-data target :static-draw arr)
    (gl:free-gl-array arr)))

(defmethod initialize-instance :after ((obj vx-buffer) &key data (size (length data)))
  (with-slots (id) obj
    (setf id (gl:gen-buffer))
    (gl:bind-buffer :array-buffer id)
    (alloc-gl-array data size :array-buffer)))

(defun make-ix-buffer (data &optional (count (length data)))
  (let ((arr (gl:alloc-gl-array :uint count)))
    (dotimes (i count)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defmethod buffer-bind ((obj vx-buffer))
  (with-slots (id) obj (gl:bind-buffer :array-buffer id)))

(defmethod buffer-unbind ((obj vx-buffer))
  (gl:bind-buffer :array-buffer 0))

(defmacro with-frame-time ((fstart fend) &body body)
  `(let ((,fstart 0.0)
	 (,fend 0.0))
     (progn
       (setf ,fstart (glfw:get-time))
       ,@body
       (setf ,fend (- (glfw:get-time) fstart)))))

(defparameter *cube-positions*
  #(#( 0.0  0.0  0.0)
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

(defun draw (va ib shader)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (buffer-bind va)
  (gl:use-program shader)

  (with-camera-position (x y z) *camera*
    (setf x (* 10 (sin (glfw:get-time)))
	  z (* 10 (cos (glfw:get-time)))))
  
  (let* ((fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (camera-view *camera*)))
    (shader-set-mat4 shader "u_view" view)
    (shader-set-mat4 shader "u_proj" projection)

    (loop for pos across *cube-positions*
	  for i by 1
	  for angle = (* 20.0 i)
	  for model = (matrix*
		       (mat4-rotate (deg->rad angle) 1.0 0.0 0.5)
		       (mat4-translate (!x pos) (!y pos) (!z pos)))
	  do
	     (shader-set-mat4 shader "u_model" (matrix* model))
	     
	     (gl:draw-elements :triangles ib))))
