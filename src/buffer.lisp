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

(defun shader-data-size (type dim)
  (case type
    (:float (* 4 dim))
    (:mat (* 4 dim dim))))

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

(defun mk-buffer-layout (&rest attributes)
  "Attributes are of the form (:type (:base-type dimension) :name attribute-name.
Example:
 (mk-buffer-layout '(:type (:float 3) :name \"a_position\")
                   '(:type (:float 4) :name \"a_color\"))."
  (let ((stride 0)
	(t-offset 0))
    (loop for e in attributes
	  for (base-type count) = (getf e :type)
	  for name = (getf e :name)
	  for size = (shader-data-size base-type count)
	  collect `(:name ,name :count ,count :type ,base-type :offset ,t-offset) into result-elems
	  do (incf t-offset size)
	  do (incf stride size)
	  finally (return `(:stride ,stride :elements ,result-elems)))))

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
  (destructuring-bind (&key stride elements) layout
    (loop for e in elements
	  for i from 0
	  do (destructuring-bind (&key name count type offset) e
	       (declare (ignore name))
	       (assert (integerp offset))
	       (gl:enable-vertex-attrib-array i)
	       (gl:vertex-attrib-pointer i count type nil stride
					 (cffi:inc-pointer (cffi:null-pointer) offset))))))
