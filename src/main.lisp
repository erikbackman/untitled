(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-glfw3")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria")
  (ql:quickload "trivial-main-thread"))

(defpackage untitled
  (:use :cl :glfw :trivial-main-thread :cl-opengl))

(in-package :untitled)

(defvar *shader-vao-vertex-program*
  "#version 330
layout (location = 0) in vec3 aPos;
void main()
{
  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
}
")

(defvar *shader-vao-fragment-program*
  "#version 330

out vec4 FragColor;
void main()
{
  FragColor = vec4(0.8f, 0.3f, 0.02f, 1.0f);
}

")

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (set-viewport w h))

(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun basic-window-example ()
  (with-body-in-main-thread ()
    (glfw:with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)

      (let* ((indicies  #(0.0 0.5 0.0 0.5 -0.5 0.0 -0.5 -0.5 0.0))
	     (buffers (gl:gen-buffers 2))
	     (vx-buffer (elt buffers 0))
	     (color-buffer (elt buffers 1))
	     (vx-array nil))

	(gl:bind-buffer :array-buffer vx-buffer)
	(let ((arr (gl:alloc-gl-array :float 9)))
	  (dotimes (i (length indicies))
	    (setf (gl:glaref arr i) (aref indicies i)))
	  (gl:buffer-data :array-buffer :static-draw arr)
	  (gl:free-gl-array arr))

	(gl:bind-buffer :array-buffer color-buffer)
	(let ((arr (gl:alloc-gl-array :float 9)))
	  (dotimes (i (length indicies))
	    (setf (gl:glaref arr i) (aref indicies i)))
	  (gl:buffer-data :array-buffer :static-draw arr)
	  (gl:free-gl-array arr))

	(setf vx-array (gl:gen-vertex-array))
	(gl:bind-vertex-array vx-array)
	(gl:bind-buffer :array-buffer vx-buffer)
	(gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
	(gl:bind-buffer :array-buffer color-buffer)
	(gl:vertex-attrib-pointer 1 3 :float nil 0 (cffi:null-pointer))
	(gl:enable-vertex-attrib-array 0)
	(gl:enable-vertex-attrib-array 1)
	
	(let ((vs (gl:create-shader :vertex-shader))
	      (fs (gl:create-shader :fragment-shader))
	      (program nil))
	  
	  (gl:shader-source vs *shader-vao-vertex-program*)
	  (gl:compile-shader vs)
	  (gl:shader-source fs *shader-vao-fragment-program*)
	  (gl:compile-shader fs)

	  (print (gl:get-shader-info-log vs))
	  (print (gl:get-shader-info-log fs))

	  (setf program (gl:create-program))
	  (gl:attach-shader program vs)
	  (gl:attach-shader program fs)
	  (gl:link-program program)

	  (loop until (window-should-close-p)
		do (gl:with-pushed-matrix
		     (gl:clear-color 0.07 0.13 0.17 1.0)
		     (gl:clear :color-buffer-bit :depth-buffer-bit)
		     (gl:use-program program)
		     (gl:draw-arrays :triangles 0 3)
		     (swap-buffers))
		do (poll-events))))))
  )

(basic-window-example)




