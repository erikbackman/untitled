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
  (speed 0.2)
  (sensitivity 0.1)
  (fov 45.0))

(defun init-camera ()
  (setf *camera* (make-camera)))

(defun camera-view (camera)
  (with-slots (position front up) camera
    (mat4-look-at position (sb-cga:vec+ position front) up)))

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

(declaim (ftype (function (single-float single-float) vec)))
(defun camera-calculate-front (pitch yaw)
  (let* ((yaw-rad (deg->rad yaw))
	 (pitch-rad (deg->rad pitch))
	 (cos-pitch (cos pitch-rad))
	 (x (* (cos yaw-rad) cos-pitch))
	 (y (sin pitch-rad))
	 (z (* (sin yaw-rad) (cos pitch-rad))))
    (sb-cga:normalize (vec x y z))))


(defun zero-vec? (v) (every #'zerop v))

(defun norm (v)
  (sb-cga:normalize
   (if (zero-vec? v)
       (sb-cga:vec+
	(sb-cga:vec single-float-epsilon single-float-epsilon single-float-epsilon) v)
       v)))

(declaim (ftype (function (camera single-float single-float))))
(defun camera-handle-mouse-movement (camera xoffset yoffset)
  (with-slots (sensitivity yaw pitch right up front world-up) camera
    (let* ((xoffset (* xoffset sensitivity))
	   (yoffset (* yoffset sensitivity))
	   (new-front (camera-calculate-front pitch yaw))
	   (new-right (norm (sb-cga:cross-product front world-up)))
	   (new-up (norm (sb-cga:cross-product right front))))

      (incf yaw xoffset)
      (incf pitch yoffset)

      (setf front new-front)
      (setf right new-right)
      (setf up new-up)
      
      )))

(defun camera-handle-keyboard (key cam)
  (with-slots (front position speed up) cam
    (case key
      (:w (vec+= position (sb-cga:vec* front speed)))
      (:s (vec+= position (sb-cga:vec* front (- speed))))
      (:a (vec+= position (sb-cga:vec* (sb-cga:normalize (sb-cga:cross-product front up)) (- speed))))
      (:d (vec+= position (sb-cga:vec* (sb-cga:normalize (sb-cga:cross-product front up)) speed)))
      (:q (vec+= position (sb-cga:vec* up speed)))
      (:e (vec+= position (sb-cga:vec* up (- speed)))))))
