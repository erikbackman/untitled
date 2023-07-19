(in-package :untitled)

(defparameter *fdelay* (/ 1000 60))

(defparameter *positions*
  #(#( 2.0  5.0 -15.0)
    #(-1.5 -2.2 -2.5)
    #(-3.8 -2.0 -12.3)
    ))

(defparameter *positions2*
  #(#( 1.5  2.0 -2.5)
    #( 1.5  0.2 -1.5)
    #(-1.3  1.0 -1.5)
    ))

(defparameter *positions3*
  #(#( 2.4 -0.4 -3.5)
    #(-1.7  3.0 -7.5)
    #( 1.3 -2.0 -2.5)
    ))

(defparameter *faded* #(0.83 0.83 0.83 0.2))
(defparameter *origin* (cg:vec 0.0 0.0 0.0))

(defun main ()
  (unwind-protect
       (with-window (:title "untitled" :width *win-w* :height *win-h*
		     :on-mouse 'handle-mouse-movement
		     :on-keyboard 'handle-key-input)
	 
	 (renderer-init)
	 (renderer-begin-scene)
	 (renderer-reset-stats)

	 (render-batch
	   (loop for pos across *positions*
		 do (draw-cube (vec-x pos) (vec-y pos) (vec-z pos)))

	   (loop for pos across *positions2*
		 do (draw-cube (vec-x pos) (vec-y pos) (vec-z pos)))

	   (loop for pos across *positions3*
		 do (draw-cube (vec-x pos) (vec-y pos) (vec-z pos)))

	   (draw-quad-rotated 0.0 0.48 0.0 90 (cg:vec 1.0 0.0 0.0) *faded* 50.0 50.0)
	   
	   (draw-line (cg:vec 0.0 -25.0 0.0) (cg:vec 0.0 25.0 0.0) *blue*)
	   (draw-line (cg:vec -25.0 0.0 0.0) (cg:vec 25.0 0.0 0.0) *red*)
	   (draw-line (cg:vec 0.0 0.0 -25.0) (cg:vec 0.0 0.0 25.0) *green*))

	 (let ((time 0.0f0)
	       (last-frame-time 0.0f0))
	   (loop until (window-should-close-p)
		 do (setf time (coerce (glfw:get-time) 'single-float))
		    (setf *timestep* (* (- time last-frame-time) 1000.0f0))
		    (setf last-frame-time time)
		 do
		    (renderer-flush)
		    (camera-handle-keyboard *camera*)
		    (swap-buffers)
		    (poll-events)))
	 
	 (shutdown))))

(main)
