(in-package :untitled)

(defparameter *fdelay* (/ 1.0 60.0))

(defparameter *positions*
  #(
    #( 2.0  5.0 -15.0)
    #(-1.5 -2.2 -2.5)
    #(-3.8 -2.0 -12.3)
    #( 2.4 -0.4 -3.5)
    ))

(defparameter *positions2*
  #(#(-1.7  3.0 -7.5)
    #( 1.3 -2.0 -2.5)
    #( 1.5  2.0 -2.5)
    #( 1.5  0.2 -1.5)
    #(-1.3  1.0 -1.5)
    ))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)
    (renderer-init)
    (renderer-begin-scene)
    (renderer-reset-stats)

    (begin-batch)
    (loop for pos across *positions*
	  do (draw-quad-at (vec-x pos) (vec-y pos) (vec-z pos) *green*))
    (loop for pos across *positions2*
	  do (draw-quad-at (vec-x pos) (vec-y pos) (vec-z pos) *red*))
    (next-batch)

    (loop until (window-should-close-p) do
      (swap-buffers)
      (poll-events))))




