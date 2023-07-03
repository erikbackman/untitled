(in-package :untitled)

(defun set-viewport (width height)
  (gl:viewport 0 0 width height))

(defun init-gl ()
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1.0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest))

(defparameter *zoom-value* 1.0)

(defmacro with-window ((&key title width height) &body body)
  `(with-body-in-main-thread ()
     (def-window-size-callback update-viewport (window w h)
       (declare (ignore window))
       (set-viewport w h))

     (def-key-callback quit-on-escape (window key scancode action mod-keys)
       (declare (ignore window scancode mod-keys))
       (when (and (eq key :escape) (eq action :press))
	 (glfw:set-window-should-close)))

     (def-scroll-callback zoom-on-scroll (window x-offset y-offset)
       (declare (ignore window x-offset))
       (if (plusp y-offset) (incf *zoom-value* 0.1)
	   (decf *zoom-value* 0.1)))

     (with-init-window (:title ,title :width ,width :height ,height)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       (set-key-callback 'quit-on-escape)
       (set-scroll-callback 'zoom-on-scroll)
       (set-window-size-callback 'update-viewport)
       (glfw:swap-interval 1)		; vsync
       (init-gl)
       (set-viewport ,width ,height)
       ,@body)))


(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width 600 :height 400)
    (let* ((shape (make-cube))
	   (vx-buffer (make-instance 'vx-buffer
				     :data (sd-verts shape)))

	   (ix-buffer (make-ix-buffer (sd-inds shape)))

	   (shader (match (load-shader "shader.glsl")
		     ((shader-src :vs vs :fs fs) (create-shader vs fs)))))
      
      ;; TODO: vertex-array abstraction
      (let ((stride (* 6 (cffi:foreign-type-size :float))))
	(gl:enable-vertex-attrib-array 0)
	(gl:vertex-attrib-pointer 0 3 :float nil stride (cffi:null-pointer))
       
	(gl:enable-vertex-attrib-array 1)
	(gl:vertex-attrib-pointer 1 3 :float nil stride
				  (cffi-sys:inc-pointer
				   (cffi:null-pointer)
				   (* 3 (cffi:foreign-type-size :float)))))
      

      (loop until (window-should-close-p)
	    do (draw vx-buffer ix-buffer shader)
	       (swap-buffers)
	       (poll-events))

      (gl:delete-program shader))))

(main)
