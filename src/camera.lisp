(in-package :untitled)

(defparameter *camera* nil)

(defstruct camera
  (position (vec 0.1 0.1 0.1))
  (front (vec 0.0 0.0 -1.0))
  (up (vec 0.0 1.0 0.0))
  (right (vec 0.0 0.0 0.0))
  (world-up (vec 0.0 1.0 0.0))
  (yaw -90.0)
  (pitch 0.1)
  (speed 0.02)
  (sensitivity 0.1)
  (fov 45.0))

(defun init-camera ()
  (setf *camera* (make-camera)))

(defun camera-view (camera)
  (with-slots (position front up) camera
    (mat4-look-at position (cg:vec+ position front) up)))

(defun camera-projection (camera aspect)
  (with-slots (fov) camera
    (mat4-perspective (deg->rad fov) aspect 0.1 100.0)))

(defun camera-view-spinny (camera)
  (with-slots (position front up sensitivity speed) camera
    (mat4-look-at `#(,(* 10 (sin (* speed (glfw:get-time))))
		     0.0
		     ,(* 10 (cos (* speed (glfw:get-time)))))
		  #(0 0 0)
		  #(0 1 0))))

(defmacro with-camera-position ((x y z) camera &body body)
  (let ((cam (gensym)))
    `(let* ((,cam ,camera))
       (with-slots (position) ,cam
	 (declare (sb-pcl::%variable-rebinding cam camera))
	 (symbol-macrolet ((,x (aref position 0))
			   (,y (aref position 1))
			   (,z (aref position 2)))
	   ,@body)))))


#|================================================================================|#
#| Mouse movement                                                                 |#
#|================================================================================|#

(defun zero-vec? (v) (every #'zerop v))

(defun norm (v)
  (cg:normalize
   (if (zero-vec? v)
       (cg:vec+
	(cg:vec single-float-epsilon single-float-epsilon single-float-epsilon) v)
       v)))

(declaim (ftype (function (single-float single-float) vec)))
(defun camera-calculate-front (pitch yaw)
  (let* ((yaw-rad (deg->rad yaw))
	 (pitch-rad (deg->rad pitch))
	 (cos-pitch (cos pitch-rad))
	 (x (* (cos yaw-rad) cos-pitch))
	 (y (sin pitch-rad))
	 (z (* (sin yaw-rad) (cos pitch-rad))))
    (cg:normalize (vec x y z))))

(declaim (ftype (function (camera single-float single-float))))
(defun camera-handle-mouse-movement (camera xoffset yoffset)
  (with-slots (sensitivity yaw pitch right up front world-up) camera
    (incf yaw (* xoffset sensitivity))
    (incf pitch (* yoffset sensitivity))
    (setf front (camera-calculate-front pitch yaw))
    (setf right (norm (cg:cross-product front world-up)))
    (setf up (norm (cg:cross-product right front)))))

#|================================================================================|#
#| Keyboard input                                                                 |#
#|================================================================================|#

(defun camera-handle-keyboard (cam)
  (with-slots (front position speed up) cam
    (let ((s (* *timestep* speed)))
      (when (keydown? :w) (vec+= position (cg:vec* front s)))
      (when (keydown? :s) (vec+= position (cg:vec* front (- s))))
      (when (keydown? :a) (vec+= position (cg:vec* (cg:normalize (cg:cross-product front up)) (- s))))
      (when (keydown? :d) (vec+= position (cg:vec* (cg:normalize (cg:cross-product front up)) s)))
      (when (keydown? :e) (vec+= position (cg:vec* up s)))
      (when (keydown? :q) (vec+= position (cg:vec* up (- s)))))))
