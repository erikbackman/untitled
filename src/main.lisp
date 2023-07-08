(in-package :untitled)

(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)

    (let* ((shape (make-cube))
	   (vx-buffer (make-instance 'vx-buffer :data (sd-verts shape)))
	   (ix-buffer (make-instance 'ix-buffer :data (sd-inds shape)))
	   (va (make-instance 'vertex-array))
	   (layout (make-vb-layout))
	   (shader (with-slots (vs fs) (load-shader "shader.glsl")
		     (create-shader vs fs))))

      (vb-layout-push layout 3)
      (vertex-array-add-buffer va vx-buffer layout)
      (loop until (window-should-close-p)
	    do (draw va ix-buffer shader)
	       (swap-buffers)
	       (let ((err (gl:check-error)))
		 (when err
		   (print (format nil "GL ERROR: ~a" err))))
	       (poll-events))

      (gl:delete-program shader))))

(main)
