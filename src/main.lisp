(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-glfw3")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria")
  (ql:quickload "trivial-main-thread"))

(defpackage untitled
  (:use :cl :glfw :trivial-main-thread)
  (:import-from :cl-opengl))

(in-package :untitled)

;; (defparameter *vs-src*
;;   "#version 330
;; layout (location = 0) in vec3 aPos;
;; void main()
;; {
;;   gl_Position = vec4(aPos, 1.0);
;; }
;; ")

(defparameter *vs-src*
  "#version 330 core
layout (location = 0) in vec3 aPos;   // the position variable has attribute position 0
layout (location = 1) in vec3 aColor; // the color variable has attribute position 1
  
out vec3 myColor; // output a color to the fragment shader

void main()
{
    gl_Position = vec4(aPos, 1.0);
    myColor = aColor; // set ourColor to the input color we got from the vertex data
}")

;; (defparameter *fs-src*
;;   "#version 330

;; out vec4 FragColor;
;; uniform vec4 myColor;
;; uniform vec3 iResolution;
;; void main()
;; {
;;   FragColor = myColor;
;; }

;; ")

(defparameter *fs-src*
  "#version 330 core
out vec4 FragColor;  
in vec3 myColor;
  
void main()
{
    FragColor = vec4(myColor, 1.0);
}")

(defun set-viewport (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho -50 50 -50 50 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    shader))

(defun create-shader (vs-src fs-src)
  (let ((program (gl:create-program))
	(vs (compile-shader :vertex-shader vs-src))
	(fs (compile-shader :fragment-shader fs-src)))
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    (gl:validate-program program)
    (gl:delete-shader vs)
    (gl:delete-shader fs)
    program))

(defun main ()
  (glfw:def-window-size-callback update-viewport (window w h)
    (declare (ignore window))
    (set-viewport w h))

  (glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
    (declare (ignore window scancode mod-keys))
    (when (and (eq key :escape) (eq action :press))
      (glfw:set-window-should-close)))
  
  (with-body-in-main-thread ()
    (glfw:with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)

      (let* ((verticies  #( 0.0  0.5 0.0  1.0 0.0 0.0
			    0.5 -0.5 0.0  0.0 1.0 0.0
			   -0.5 -0.5 0.0  0.0 0.0 1.0))
	     (buffers (gl:gen-buffers 2))
	     (vx-buffer (elt buffers 0))
	     (color-buffer (elt buffers 1))
	     (vx-array nil))

	(gl:bind-buffer :array-buffer vx-buffer)
	(let ((arr (gl:alloc-gl-array :float 24)))
	  (dotimes (i (length verticies))
	    (setf (gl:glaref arr i) (aref verticies i)))
	  (gl:buffer-data :array-buffer :static-draw arr)
	  (gl:free-gl-array arr))

	(gl:bind-buffer :array-buffer color-buffer)
	(let ((arr (gl:alloc-gl-array :float 24)))
	  (dotimes (i (length verticies))
	    (setf (gl:glaref arr i) (aref verticies i)))
	  (gl:buffer-data :array-buffer :static-draw arr)
	  (gl:free-gl-array arr))

	(setf vx-array (gl:gen-vertex-array))
	(gl:bind-vertex-array vx-array)
	(gl:bind-buffer :array-buffer vx-buffer)
	(gl:vertex-attrib-pointer 0 3 :float nil 24 (cffi:null-pointer))
	(gl:bind-buffer :array-buffer color-buffer)
	(gl:vertex-attrib-pointer 1 3 :float nil 24 (cffi:inc-pointer (cffi:null-pointer) 12))
	(gl:enable-vertex-attrib-array 0)
	(gl:enable-vertex-attrib-array 1)

	;; Create shaders
	(let* ((program (create-shader *vs-src* *fs-src*)))
	  (loop until (window-should-close-p)
		;; with loc = (gl:get-uniform-location program "myColor")
		;; for tv = (glfw:get-time)
		;; for green = (/ (sin tv) (+ 2.0 0.5))
		;; for red = 1.0
		;; for blue = 1.0

		do (gl:with-pushed-matrix
		     (progn (gl:clear-color 0.07 0.13 0.17 1.0)
			    (gl:clear :color-buffer-bit :depth-buffer-bit)
			    (gl:use-program program)
			    ;;(%gl:uniform-4f loc red green blue 1.0)
			    (gl:draw-arrays :triangles 0 3)
			    (swap-buffers)))
		do (poll-events)))))))

(main)






