(in-package :untitled)

(defparameter *fdelay* (/ 1000 60))

(defparameter *pink* #(0.92 0.09 0.83 1.0))
(defparameter *white* #(1.0 1.0 1.0 1.0))
(defparameter *red* #(1.0 0.0 0.0 1.0))
(defparameter *green* #(0.0 1.0 0.0 1.0))
(defparameter *blue* #(0.0 0.0 1.0 1.0))
(defparameter *cyan* #(0.0 1.0 1.0 0.4))
(defparameter *faded* #(0.95 0.95 0.95 0.6))
(defparameter *black* #(0.0 0.0 0.0 1.0))
(defparameter *dusk-blue* #(0.30 0.42 0.47 1.0))
(defparameter *color1* #(1.0 1.0 1.0 0.2))

(defparameter *e1* (vec 1.0 0.0 0.0))
(defparameter *e2* (vec 0.0 1.0 0.0))
(defparameter *e3* (vec 0.0 0.0 1.0))

;; TODO: Shouldn't depend on glfw.
(defun main ()
  (with-window (:title "untitled" :width 800 :height 600
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)
    
    (renderer-init)
    (renderer-reset-stats)
    (renderer-set-clear-color *dusk-blue*)

    (scene-set
     '((draw-line (vec -25.0 0.0 0.0) (vec 25.0 0.0 0.0) *red*)
       (draw-line (vec 0.0 0.0 -25.0) (vec 0.0 0.0 25.0) *green*)
       (draw-line (vec 0.0 -25.0 0.0) (vec 0.0 25.0 0.0) *blue*)
       (draw-quad-rotated 0.0 0.5 0.0 90 (vec 1.0 0.0 0.0) #(1.0 1.0 1.0 0.2) 50.0 50.0)))
    
    (scene-submit)
    
    (let ((time 0.0f0)
	  (last-frame-time 0.0f0))
      (loop until (glfw:window-should-close-p)
	    do (setf time (coerce (glfw:get-time) 'single-float))
	       (setf *timestep* (* (- time last-frame-time) 1000.0f0))
	       (setf last-frame-time time)
	    do
	       (renderer-present)
	       (camera-handle-keyboard *camera*)
	       (glfw:swap-buffers)
	       (glfw:poll-events)))
    
    (shutdown)))

(main)
