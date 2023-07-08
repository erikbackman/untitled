(in-package :untitled)

(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)

    (let* ((shape (make-cube))
	   (vb (make-instance 'vertex-buffer :data (sd-verts shape)))
	   (ib (make-instance 'index-buffer :data (sd-inds shape)))
	   (va (make-instance 'vertex-array))
	   (layout (make-vertex-buffer-layout))
	   (shader (with-slots (vs fs) (load-shader "shader.glsl")
		     (create-shader vs fs))))

      (vertex-buffer-layout-push layout 3)
      (vertex-array-add-buffer va vb layout)
      (loop until (window-should-close-p)
	    do (draw va ib shader)
	       (swap-buffers)
	       (let ((err (gl:check-error)))
		 (when err
		   (print (format nil "GL ERROR: ~a" err))))
	       (poll-events))

      (gl:delete-program shader))))

(main)
