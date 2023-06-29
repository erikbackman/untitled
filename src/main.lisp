(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-glfw3")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria")
  (ql:quickload "trivial-main-thread"))

(defpackage untitled
  (:use :cl :glfw :trivial-main-thread)
  (:import-from :cl-opengl)
  (:import-from :alexandria :switch))

(in-package :untitled)

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmacro my-with-window (() &body body)
  `(with-body-in-main-thread ()
     (def-window-size-callback update-viewport (window w h)
       (declare (ignore window))
       (set-viewport w h))

     (def-key-callback quit-on-escape (window key scancode action mod-keys)
       (declare (ignore window scancode mod-keys))
       (when (and (eq key :escape) (eq action :press))
	 (set-window-should-close)))
     
     (glfw:with-init-window (:title "untitled" :width 600 :height 400)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       (set-key-callback 'quit-on-escape)
       (set-window-size-callback 'update-viewport)
       (gl:clear-color 0 0 0 0)
       (set-viewport 600 400)
       ,@body)))

(defun main ()
  (my-with-window ()
    (let* ((verticies #(0.0 0.5 0.0
			0.5 -0.5 0.0
			-0.5 -0.5 0.0))
	   (vx-buffer (make-instance 'vx-buffer :data verticies :size (length verticies)))
	   (ix-buffer (make-ix-buffer 3))
	   (src (load-shader "shader.glsl"))
	   (shader (create-shader (shader-src-vs src)
				  (shader-src-fs src))))

      ;; TODO: vertex-array
      (gl:vertex-attrib-pointer 0 3 :float nil (* 3 (cffi:foreign-type-size :float)) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 0)

      (loop until (glfw:window-should-close-p)
	    do (gl:with-pushed-matrix
		 (gl:clear-color 0.07 0.13 0.17 1.0)
		 (gl:clear :color-buffer-bit :depth-buffer-bit)
		 (draw vx-buffer ix-buffer shader)
		 (swap-buffers))
	    do (poll-events))
      
      (gl:delete-program shader))))

(main)

