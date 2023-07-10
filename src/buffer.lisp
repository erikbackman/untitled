(in-package :untitled)

(defun alloc-gl-array (data size target)
  (let ((arr (gl:alloc-gl-array :float size)))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:buffer-data target :static-draw arr)
    (gl:free-gl-array arr)))

(defgeneric bind (obj))
(defgeneric unbind (obj))

#|================================================================================|# 
#| Vertex Buffer                                                                  |# 
#|================================================================================|# 

(defclass vertex-buffer ()
  ((id :accessor id)
   (layout :accessor layout)))

(defmethod initialize-instance :after ((obj vertex-buffer) &key data (size (length data)))
  (with-slots (id) obj
    (setf id (gl:gen-buffer))
    (gl:bind-buffer :array-buffer id)
    (alloc-gl-array data size :array-buffer)))

(defmethod bind ((obj vertex-buffer))
  (with-slots (id) obj (gl:bind-buffer :array-buffer id)))

(defmethod unbind ((obj vertex-buffer))
  (gl:bind-buffer :array-buffer 0))

(defun set-layout (vertex-buffer layout)
  (setf (slot-value vertex-buffer 'layout) layout))


#|================================================================================|# 
#| Index Buffer                                                                   |# 
#|================================================================================|# 

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

(defmethod bind ((obj index-buffer))
  (with-slots (id) obj (gl:bind-buffer :element-array-buffer id)))

(defmethod unbind ((obj index-buffer))
  (gl:bind-buffer :element-array-buffer 0))


#|================================================================================|# 
#| Vertex Layout                                                                  |# 
#|================================================================================|# 

(defun shader-data-size (type)
  (case type
    (:float 4)
    (:float3 (* 4 3))
    (:float4 (* 4 4))
    (:mat3 (* 4 3 3))
    (:mat4 (* 4 4 4))))

(defstruct (buffer-element
	    (:constructor make-buffer-element
		(type name &optional (offset 0)
		 &aux (size (shader-data-size type)))))
  name type size offset)

(defun buffer-element-count (element)
  (with-slots (type) element
    (case type
      (:float3 3)
      (:float4 4))))

(defun calculate-offset-and-stride (layout)
  (let ((t-offset 0))
    (with-slots (elements stride) layout
      (loop for e in elements do
	(with-slots (offset size) e
	  (setf offset t-offset)
	  (incf t-offset size)
	  (incf stride size))))))

(defclass buffer-layout ()
  ((elements)
   (stride :initform 0)))

(defmethod initialize-instance :after ((obj buffer-layout) &key elements)
  (setf (slot-value obj 'elements) elements)
  (calculate-offset-and-stride obj))

#|================================================================================|# 
#| Vertex Array                                                                   |# 
#|================================================================================|# 

(defclass vertex-array ()
  ((id :initarg :name
       :accessor id
       :initform (gl:gen-vertex-array))
   (vertex-buffers :accessor vertex-bufffers)
   (index-buffer :accessor index-buffer)))

(defmethod bind ((obj vertex-array))
  (with-slots (id) obj (gl:bind-vertex-array id)))

(defmethod unbind ((obj vertex-array))
  (gl:bind-vertex-array 0))

(defun set-index-buffer (va ib)
  (bind va)
  (bind ib)
  (setf (slot-value va 'index-buffer) ib))

(defun add-vertex-buffer (va vb layout)
  (bind va)
  (bind vb)
  (with-slots (elements stride) layout
      (loop for e in elements
	    for i from 0 below (length elements)
	    for cnt = (buffer-element-count e)
	    for off = (slot-value e 'offset)
	    do
	       (gl:enable-vertex-attrib-array i)
	       (gl:vertex-attrib-pointer
		i cnt :float nil stride (cffi-sys:inc-pointer (cffi:null-pointer) off)))))
