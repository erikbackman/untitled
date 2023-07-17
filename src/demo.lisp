(in-package :untitled)

;; This doesn't work with the current changes.

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count 18))

(defparameter *cube-positions*
  #(
    #( 2.0  5.0 -15.0)
    #(-1.5 -2.2 -2.5)
    #(-3.8 -2.0 -12.3)
    #( 2.4 -0.4 -3.5)
    #(-1.7  3.0 -7.5)
    #( 1.3 -2.0 -2.5)
    #( 1.5  2.0 -2.5)
    #( 1.5  0.2 -1.5)
    #(-1.3  1.0 -1.5)
    ))

(defun draw-demo (va ib shader)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program shader)
  (let* ((fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (matrix*
		(mat4-translate 0.0 0.0 10.0)
		(camera-view *camera*))))

    (shader-set-mat4 shader "u_view" view)
    (shader-set-mat4 shader "u_proj" projection)

    (set-index-buffer va ib)

    (let ((time (* 2 (glfw:get-time))))
      (loop for pos across *cube-positions*
	    for i by 1
	    for angle = (* time i)
	    do
	       (shader-set-mat4 shader "u_model"
				(matrix* (mat4-rotate (deg->rad angle) 1.0 0.0 0.5)
					 (mat4-translate (vec-x pos) (vec-y pos) (vec-z pos))))

	       (draw-triangles ib)))))

(defun run-demo ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*
		:on-mouse 'handle-mouse-movement
		:on-keyboard 'handle-key-input)

    (let* ((shape (make-cube))
	   (vb (make-instance 'vertex-buffer :data (sd-verts shape)))
	   (ib (make-instance 'index-buffer :data (sd-inds shape)))
	   (va (make-instance 'vertex-array))
	   
	   (layout (mk-buffer-layout '(:type (:float 3) :name "a_position")
				     '(:type (:float 4) :name "a_color")))
	   
	   (shader (with-slots (vs fs) (load-shader "shader.glsl")
		     (create-shader vs fs))))

      (add-vertex-buffer va vb layout)
      (loop until (window-should-close-p)
	    do (draw-demo va ib shader)
	       (swap-buffers)
	       (let ((err (gl:check-error)))
		 (when err
		   (print (format nil "GL ERROR: ~a" err))))
	       (poll-events))

      (gl:delete-program shader))))
