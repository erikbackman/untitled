(in-package :g3d)

(defparameter *default-scene* '())

(defstruct scene
  (dirty nil)
  (environment (make-hash-table))
  (objects (make-hash-table)))

(defparameter *scene* (make-scene))
(defparameter *scene-update-callbacks* '())

(defun scene-set (scene)
  (setf *default-scene* scene))

(defun scene-dirty? ()
  (slot-value *scene* 'dirty))

(defun current-scene-objects ()
  (scene-objects *scene*))

(defun scene-object-count ()
  (hash-table-count (current-scene-objects)))

(defun scene-current-environment ()
  (scene-environment *scene*))

(defun scene-get-variable (name)
  (gethash name (scene-current-environment)))

(defun scene-set-variable (var value)
  (setf (gethash var (scene-current-environment)) value))

(defun scene-make-dirty ()
  (setf (slot-value *scene* 'dirty) t))

(defun scene-add (draw-function)
  (let ((id (scene-object-count)))
    (setf (gethash id (current-scene-objects)) draw-function)
    (scene-make-dirty)
    id))

(defun scene-remove (id)
  (remhash id (current-scene-objects))
  (scene-make-dirty))

(defun scene-clear ()
  (let ((h (current-scene-objects)))
    (maphash #'(lambda (key val) (declare (ignore val))
		 (remhash key h))
	     h))
  (scene-make-dirty))

(defun scene-submit ()
  (let ((calls *default-scene*))
    (maphash (lambda (key val)
	       (declare (ignore key))
	       (push val calls))
	     (current-scene-objects))
    (eval `(renderer-begin-scene ,@calls))))
