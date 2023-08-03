(in-package :untitled)

;; TODO: Shouldn't depend on glfw.
(defun main ()
  (with-window (:title "untitled" :width 800 :height 600
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)
    
    (renderer-init)
    (renderer-reset-stats)
    (renderer-set-clear-color g3d:*dusk-blue*)

    (scene-add (get-current-scene)
      (geometry :l1 'line :start #(-25.0 0.0 0.0) :end #(25.0 0.0 0.0) :color g3d:*red*)
      (geometry :l2 'line :start (vec 0.0 0.0 -25.0) :end (vec 0.0 0.0 25.0) :color g3d:*green*)
      (geometry :l3 'line :start (vec 0.0 -25.0 0.0) :end (vec 0.0 25.0 0.0) :color g3d:*blue*)
      (geometry :pl1 'plane :normal (vec 0.0 1.0 0.0) :scale (vec 50.0 50.0 50.0) :color g3d:*faded*))

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

