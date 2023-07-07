(in-package :untitled)

(defparameter *camera* nil)

(defstruct camera
  position
  front up right world-up
  yaw pitch
  speed sensitivity fov)

(defun init-camera
    (&key
       (position #(0.0 0.0 0.0))
       (up #(0.0 1.0 0.0))
       (yaw -90.0)
       (pitch 0.1)
       (fov 45)
       (speed 0.09)
       (sensitivity 0.1))
  (setf *camera*
	(make-camera
	 :position position
	 :front #(0.0 0.0 -1.0)
	 :right #(0.0 0.0 0.0)
	 :up up
	 :world-up up
	 :yaw yaw
	 :pitch pitch
	 :fov fov
	 :speed speed
	 :sensitivity sensitivity)))

(defun camera-view (camera)
  (with-slots (position front up) camera
      (tr-look-at position (vec+ position front) up)))

(defun camera-view-spinny (camera)
  (with-slots (position front up) camera
    (tr-look-at position #(0 0 0) #(0 1 0))))

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
