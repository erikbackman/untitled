(in-package :g3d)

(defparameter *timestep* 0.0)

(defparameter *input-state* nil)

(defun set-key-state (key state)
  (setf (getf *input-state* key) state))

(defun keydown? (key) (getf *input-state* key))
