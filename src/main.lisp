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
		 do (draw-quad-at (vec-x pos) (vec-y pos) (vec-z pos) *green*))
	  
	   (loop for pos across *positions2*
		 do (draw-quad-at (vec-x pos) (vec-y pos) (vec-z pos) *red*))
	  
	   (loop for pos across *positions3*
		 do (draw-quad-at (vec-x pos) (vec-y pos) (vec-z pos) *blue*)))
	
	 (loop until (window-should-close-p)
	       do
		  (renderer-flush)

		  (swap-buffers)
		  (poll-events))
	 (shutdown))))

(main)
