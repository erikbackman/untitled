(in-package :g3d)

(defstruct scene
  (dirty nil)
  (environment (make-hash-table))
  (objects (make-hash-table)))

(defparameter *current-scene* (make-scene))

(defun geometry (id shape &rest spec)
  (apply #'make-instance shape :id id spec))

(defun get-current-scene ()
  (or *current-scene*
      (setf *current-scene* (make-scene))))

(defun make-scene-dirty (scene)
  (setf (slot-value scene 'dirty) t))

(defun scene-dirty? ()
  (slot-value *current-scene* 'dirty))

(defun get-variable (var scene)
  (gethash var (slot-value scene 'environment)))

(defun set-variable (var value scene)
  (setf (gethash var (slot-value scene 'environment)) value)
  (make-scene-dirty scene))

(defun get-objects (scene)
  (slot-value scene 'objects))

(defun get-object-by-id (id scene)
  (gethash id (get-objects scene)))

(defun lookup-slot-value (obj slot)
  (let ((value (slot-value obj slot)))
    (if (symbolp value) 
	(get-variable value (slot-value obj 'scene))
	value)))

(defun scene-set (&rest objects)
  (dolist (obj objects)
    (scene-add obj *current-scene*)))

(defun scene-clear (scene)
  (clrhash (scene-objects scene))
  (make-scene-dirty scene))

(defun scene-add (scene &rest objects)
  (flet ((add (obj)
	   (setf (gethash (slot-value obj 'id)
			  (slot-value scene 'objects))
		 obj)))
    (dolist (obj objects) (add obj))
    (make-scene-dirty scene)))

(defun scene-delete (scene id)
  (remhash id (get-objects scene))
  (make-scene-dirty scene))

(defgeneric draw (obj))

(defclass object ()
  ((id :initarg :id :accessor id)
   (scene
    :initarg :scene
    :accessor scene
    :initform (get-current-scene))))

(defclass quad (object)
  ((position
    :initarg :position
    :initform (vec 0.0 0.0 0.0))
   (scale
    :initarg :scale
    :initform 1.0
    :accessor scale)))

(defclass cube (object)
  ((position
    :initarg :position
    :initform (vec 0.0 0.0 0.0))))

(defclass sphere (object)
  ((radius
    :initarg :radius
    :initform 1.0
    :accessor radius)
   (color
    :initarg :color
    :initform *white*
    :accessor color)))

(defclass plane (object)
  ((normal
    :initarg :normal
    :initform (vec 1.0 1.0 1.0)
    :accessor normal)
   (point
    :initarg :point
    :initform (vec 0.0 0.0 0.0)
    :accessor point)
   (scale
    :initarg :scale
    :initform (vec 25.0 25.0 25.0)
    :accessor scale)
   (color
    :initarg :color
    :initform g3d:*cyan*
    :accessor color)))

(defclass line (object)
  ((color
    :initarg :color
    :initform #(1.0 1.0 1.0 1.0)
    :accessor color)
   (start
    :initarg :start
    :initform (vec 0.0 0.0 0.0)
    :accessor start)
   (end
    :initarg :end
    :initform (vec 0.0 0.0 0.0)
    :accessor end)))

(defmethod draw ((obj sphere))
  (draw-sphere (lookup-slot-value obj 'radius)))

(defmethod draw ((obj plane))
  (draw-plane-normal (lookup-slot-value obj 'normal)
		     (slot-value obj 'point)
		     (slot-value obj 'scale)
		     (slot-value obj 'color)))

(defmethod draw ((obj cube))
  (with-vec3 (x y z) (lookup-slot-value obj 'position)
    (draw-cube x y z)))

(defmethod draw ((obj line))
  (draw-line
   (slot-value obj 'start)
   (slot-value obj 'end)
   (slot-value obj 'color)))
