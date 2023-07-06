(in-package :untitled)

(defparameter *camera* nil)

(defstruct camera
  pos
  front
  up
  fov
  speed)

(defun init-camera ()
  (setf *camera*
	(make-camera
	 :pos #(0.0 0.0 0.5)
	 :front #(0.0 0.0 -1.0)
	 :up #(0.0 1.0 0.0)
	 :speed 0.09
	 :fov 45)))
