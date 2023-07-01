(in-package :untitled)

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun init-gl ()
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1.0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest))

(defmacro with-window ((&key title width height) &body body)
  `(with-body-in-main-thread ()
     (def-window-size-callback update-viewport (window w h)
       (declare (ignore window))
       (set-viewport w h))

     (def-key-callback quit-on-escape (window key scancode action mod-keys)
       (declare (ignore window scancode mod-keys))
       (when (and (eq key :escape) (eq action :press))
	 (glfw:set-window-should-close)))

     (with-init-window (:title ,title :width ,width :height ,height)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       (set-key-callback 'quit-on-escape)
       (set-window-size-callback 'update-viewport)
       (glfw:swap-interval 1)		; vsync
       (init-gl)
       (set-viewport ,width ,height)
       ,@body)))


(defun main ()
  (with-window (:title "untitled" :width 600 :height 400)
    (let ((vx-buffer (make-instance 'vx-buffer
				    :data #( 0.5  0.5 0.0
					     0.5 -0.5 0.0
					    -0.5 -0.5 0.0
					    -0.5  0.5 0.0 )))
	   
	  (ix-buffer (make-ix-buffer #(0 1 2
				       2 3 0)
				     6))

	  (shader (match (load-shader "shader.glsl")
		    ((shader-src :vs vs :fs fs) (create-shader vs fs)))))

      ;; TODO: vertex-array abstraction
      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 (cffi:foreign-type-size :float)) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)

      (loop until (window-should-close-p)
	    do (with-frame-time (fstart fend)
		 (draw vx-buffer ix-buffer shader)
		 (swap-buffers)
		 (poll-events)
		 (let ((fdelay (/ 1.0 30.0)))
		   (when (> fdelay fend) (sleep (- fdelay fend))))
		 ))
      
      (gl:delete-program shader))))

(main)
