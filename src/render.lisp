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

(defmethod initialize-instance :after ((obj vx-buffer) &key data size)
  (with-slots (id) obj
    (setf id (gl:gen-buffer))
    (gl:bind-buffer :array-buffer id)
    (alloc-gl-array data size :array-buffer)))

(defun make-ix-buffer (data count)
  (let ((arr (gl:alloc-gl-array :uint count)))
    (dotimes (i count)
      (setf (gl:glaref arr i) (aref data i)))
    arr))

(defmethod buffer-bind ((obj vx-buffer))
  (with-slots (id) obj (gl:bind-buffer :array-buffer id)))

(defmethod buffer-unbind ((obj vx-buffer))
  (gl:bind-buffer :array-buffer 0))

(defun draw (va ib shader)
  (buffer-bind va)
  (gl:use-program shader)
  (gl:draw-elements :triangles ib))
