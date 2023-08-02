(in-package :g3d)

(defstruct scene
  (dirty nil)
  (environment (make-hash-table))
  (objects (make-hash-table)))

(defparameter *current-scene* (make-scene))

(defun scene-dirty? ()
  (slot-value *current-scene* 'dirty))

(defun get-variable (var scene)
  (gethash var (slot-value scene 'environment)))

(defun set-variable (var value scene)
  (setf (gethash var (slot-value scene 'environment)) value)
  (setf (slot-value scene 'dirty) t))

(defun get-objects (scene)
  (slot-value scene 'objects))

(defgeneric draw (obj))

(defclass object ()
  ((scene
    :initarg :scene
    :accessor scene)))

(defclass quad (object)
  ((position
    :initarg :position
    :initform (vec 0.0 0.0 0.0)
    :type (or symbol vec))
   (scale
    :initarg :scale
    :initform 1.0
    :accessor scale)))

(defclass cube (object)
  ((position
    :initarg :position
    :initform (vec 0.0 0.0 0.0)
    :type (or symbol vec))))

(defclass sphere (object)
  ((radius
    :initarg :radius
    :initform 1.0
    :accessor radius
    :type (or symbol single-float))))

(defclass plane (object)
  ((normal
    :initarg :normal
    :initform (vec 1.0 1.0 1.0)
    :accessor normal
    :type (or symbol vec))
   (point
    :initarg :point
    :initform (vec 0.0 0.0 0.0)
    :accessor point
    :type (or symbol vec))
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

(defun scene-add (obj scene)
  ;; Would use a user supplied name/id later
  (setf (gethash (hash-table-count (slot-value scene 'objects))
		 (slot-value scene 'objects))
	obj)
  (setf (slot-value obj 'scene) scene)
  (setf (slot-value scene 'dirty) t))

(defun lookup-slot-value (obj slot)
  (let ((value (slot-value obj slot)))
    (case (type-of value)
      (symbol (get-variable value (slot-value obj 'scene)))
      (t value))))

(defmethod draw ((obj sphere))
  (draw-sphere (lookup-slot-value obj 'radius)))

(defmethod draw ((obj plane))
  (draw-plane-normal (lookup-slot-value obj 'normal)
		     *origin*
		     (slot-value obj 'scale)
		     (slot-value obj 'color)))

(defmethod draw ((obj cube))
  (with-vec3 (x y z) (lookup-slot-value obj 'position)
    (draw-cube x y z)))

(defmethod draw ((obj line))
  (draw-line (lookup-slot-value obj 'start)
	     (lookup-slot-value obj 'end)
	     (slot-value obj 'color)))

(defun scene-set (&rest objects)
  (dolist (obj objects)
    (scene-add obj *current-scene*)))

(defun scene-submit ()
  (renderer-begin-scene
    (dolist (obj (alexandria:hash-table-values (scene-objects *current-scene*)))
      (draw obj))))
