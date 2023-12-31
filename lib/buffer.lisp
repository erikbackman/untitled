(in-package :g3d)

(defun alloc-gl-array (data size target)
  (let ((arr (gl:alloc-gl-array :float size)))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:buffer-data target :dynamic-draw arr)
    (gl:free-gl-array arr)))

(defgeneric bind (obj))
(defgeneric unbind (obj))
(defgeneric delete-buffer (obj))

#|================================================================================|# 
#| Vertex Buffer                                                                  |# 
#|================================================================================|# 

(defclass vertex-buffer ()
  ((id :accessor id)
   (layout :accessor layout)
;;   (data :accessor data :initform (make-array '(0) :fill-pointer 0 :adjustable t))
   ))

(defmethod initialize-instance :after ((obj vertex-buffer) &key data (size (array-total-size data)))
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
    (gl:bind-buffer :array-buffer id)
    (let ((arr (gl:alloc-gl-array :unsigned-int count)))

      (dotimes (i count)
	(setf (gl:glaref arr i) (aref data i)))
      
      (gl:buffer-data :array-buffer :static-draw arr)
      (gl:free-gl-array arr))))

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

(defun mk-buffer-layout (&rest attributes)
  "Attributes are of the form (:type (:base-type dimension) :name attribute-name.
Example:
 (mk-buffer-layout '(:type (:float 3) :name \"a_position\")
                   '(:type (:float 4) :name \"a_color\"))."
  (loop for a in attributes
	for (base-type count) = (getf a :type)
	collect `(:name ,(getf a :name) :count ,count :type ,base-type :offset ,offset) into elements
	sum (shader-data-size base-type count) into offset
	finally (return `(:stride ,offset :elements ,elements))))

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

(defun get-index-buffer (va)
  (slot-value va 'index-buffer))

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
					 (cffi:inc-pointer (cffi:null-pointer) offset)))))
  (unbind va))
