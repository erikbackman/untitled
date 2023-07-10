(in-package :untitled)

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

(defun draw-triangles (ib)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-int)
		    :count (slot-value ib 'count)))


(defun draw (va ib shader)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:use-program shader)
  (let* ((fov (camera-fov *camera*))
	 (projection (mat4-perspective (deg->rad fov) *aspect* 0.1 100.0))
	 (view (camera-view *camera*)))

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
