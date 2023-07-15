(in-package :untitled)

(defparameter *camera* nil)

(defstruct camera
  (position #(0.0 0.0 0.0) :type (simple-array float))
  (front #(0.0 0.0 -1.0) :type (simple-array float))
  (up #(0.0 1.0 0.0) :type (simple-array float))
  (right #(0.0 0.0 0.0) :type (simple-array float))
  (world-up #(0.0 1.0 0.0) :type (simple-array float))
  (yaw -90.0 :type float)
  (pitch 0.1 :type float)
  (speed 0.2 :type float)
  (sensitivity 0.1 :type float)
  (fov 45.0 :type float))

(defun init-camera ()
  (setf *camera* (make-camera)))

(defun camera-view (camera)
  (with-slots (position front up) camera
    (mat4-look-at position (vec+ position front) up)))

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

(defun camera-calculate-front (pitch yaw)
  (let* ((yaw-rad (deg->rad yaw))
	 (pitch-rad (deg->rad pitch))
	 (cos-pitch (cos pitch-rad))
	 (x (* (cos yaw-rad) cos-pitch))
	 (y (sin pitch-rad))
	 (z (* (sin yaw-rad) (cos pitch-rad))))
    (vec-normalize `#(,x ,y ,z))))

(defun camera-handle-mouse-movement (camera xoffset yoffset)
  (with-slots (sensitivity yaw pitch right up front world-up) camera
    (let* ((xoffset (* xoffset sensitivity))
	   (yoffset (* yoffset sensitivity))
	   (new-front (camera-calculate-front pitch yaw))
	   (new-right (vec-normalize (vec-cross front world-up)))
	   (new-up (vec-normalize (vec-cross right front))))
      (incf yaw xoffset)
      (incf pitch yoffset)
      (setf front new-front
	    right new-right
	    up new-up))))

(defun camera-handle-keyboard (key cam)
  (with-slots (front position speed up) cam
    (case key
      (:w (vec+= position (vec3* speed front)))
      (:s (vec-= position (vec3* speed front)))
      (:a (vec-= position (vec3* speed (vec-normalize (vec-cross front up)))))
      (:d (vec+= position (vec3* speed (vec-normalize (vec-cross front up)))))
      (:q (vec+= position (vec3* speed up)))
      (:e (vec+= position (vec3* speed (vec3* -1 up)))))))
