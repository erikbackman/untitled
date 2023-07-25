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

(defparameter *pink* #(0.92 0.09 0.83 1.0))

(defun draw-scene (time)
  (render-batch
    (draw-line (vec -25.0 0.0 0.0) (vec 25.0 0.0 0.0) *red*)
    (draw-line (vec 0.0 0.0 -25.0) (vec 0.0 0.0 25.0) *green*)
    (draw-line (vec 0.0 -25.0 0.0) (vec 0.0 25.0 0.0) *blue*)
    (draw-plane-points (vec 0.0 0.0 0.0) (vec 1.0 0.0 0.0) (vec 1.0 1.0 1.0))
		      
    (draw-quad-rotated 0.0 0.49 0.0 90 (vec 1.0 0.0 0.0) *faded* 50.0 50.0)
    (draw-cube -2.0 0.0 4.0)
    (draw-line *origin* (vec (* 2 (cos time)) 5.0 (* 2 (sin time))) *pink*)
    ))

(defun main ()
  (unwind-protect
       (with-window (:title "untitled" :width *win-w* :height *win-h*
		     :on-mouse 'handle-mouse-movement
		     :on-keyboard 'handle-key-input)
	 
	 (renderer-init)
	 (renderer-begin-scene)
	 (renderer-reset-stats)
	 (renderer-set-clear-color *dusk-blue*)

	 (let ((time 0.0f0)
	       (last-frame-time 0.0f0))
	   (loop until (window-should-close-p)
		 do (setf time (coerce (glfw:get-time) 'single-float))
		    (setf *timestep* (* (- time last-frame-time) 1000.0f0))
		    (setf last-frame-time time)
		 do
		    (draw-scene time)
		    
		    (camera-handle-keyboard *camera*)		    
		    (renderer-flush)
		    (swap-buffers)
		    (poll-events)))
	 
	 (shutdown))))

(main)
