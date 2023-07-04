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

(defun print-shader-source (shader)
  (print (gl:get-shader-source (elt (gl:get-attached-shaders shader) 0))))

(defmacro set-uniformf (shader name x &optional y z w)
  `(gl:uniformf (funcall 'gl:get-uniform-location ,shader ,name) ,x ,y ,z ,w))

(defmacro set-uniform-matrix4f (shader name matrix)
  `(gl:uniform-matrix-4fv (gl:get-uniform-location ,shader ,name) ,matrix nil))

(defmacro with-frame-time ((fstart fend) &body body)
  `(let ((,fstart 0.0)
	 (,fend 0.0))
     (progn
       (setf ,fstart (glfw:get-time))
       ,@body
       (setf ,fend (- (glfw:get-time) fstart)))))

(defun deg->rad (deg)
  (* deg (/ pi 180)))

(defun draw (va ib shader)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (buffer-bind va)
  (gl:use-program shader)

  (let ((rot-x (aref *rotation* 0))
	(rot-y (aref *rotation* 1)))
    (set-uniform-matrix4f
     shader "u_MVP"
     (matrix*
      ;; Projection
      (tr-mat4-perspective (deg->rad 60.0) +aspect+ *zoom* 100.0)
      ;; View
      (tr-mat4-translate 0.0 0.0 1.0)
      ;; Model
      (tr-mat4-rotate (deg->rad rot-x) 1.0 0.0 0.0)
      (tr-mat4-rotate (deg->rad rot-y) 0.0 1.0 0.0)
      (tr-mat4-rotate (deg->rad 10) 1.0 0.0 0.0))))

  (gl:draw-elements :triangles ib))
