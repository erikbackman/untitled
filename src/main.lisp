(in-package :untitled)

(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)

    (let* ((shape (make-cube))
	   (vx-buffer (make-instance 'vx-buffer :data (sd-verts shape)))
	   (ix-buffer (make-ix-buffer (sd-inds shape)))
	   (shader (with-slots (vs fs) (load-shader "shader.glsl")
		     (create-shader vs fs))))

      ;; TODO: vertex-array abstraction
      (let ((stride (* 6 (cffi:foreign-type-size :float))))

	(gl:enable-vertex-attrib-array 0)
	(gl:vertex-attrib-pointer 0 3 :float nil stride (cffi:null-pointer))

	(gl:enable-vertex-attrib-array 1)
	(gl:vertex-attrib-pointer 1 3 :float nil stride
				  (cffi-sys:inc-pointer
				   (cffi:null-pointer)
				   (* 3 (cffi:foreign-type-size :float))))
	(gl:bind-buffer :array-buffer 0)
	)
      (loop until (window-should-close-p)
	    do (draw vx-buffer ix-buffer shader)
	       (swap-buffers)
	       (poll-events))

      (gl:delete-program shader))))

(main)
