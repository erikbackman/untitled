(in-package :untitled)

(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)
    (renderer-init)
    (renderer-begin-scene)
    (render-basic-scene)
    
    (loop until (window-should-close-p)
	  do (renderer-flush)
	     (swap-buffers)
	     (poll-events))))

;;(main)

