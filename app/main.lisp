(in-package :untitled)

;; TODO: Shouldn't depend on glfw.
(defun main ()
  (with-window (:title "untitled" :width 800 :height 600
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)
    
    (renderer-init)
    (renderer-reset-stats)
    (renderer-set-clear-color *dusk-blue*)

    (scene-set
     '((draw-line (vec -25.0 0.0 0.0) (vec 25.0 0.0 0.0) g3d:*red*)
       (draw-line (vec 0.0 0.0 -25.0) (vec 0.0 0.0 25.0) g3d:*green*)
       (draw-line (vec 0.0 -25.0 0.0) (vec 0.0 25.0 0.0) g3d:*blue*)
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
