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

(defclass vx-buffer () ((id :accessor id)))

(defgeneric buffer-bind (obj))
(defgeneric buffer-unbind (obj))

(defun alloc-gl-array (data size target)
  (let ((arr (gl:alloc-gl-array :float size)))
    (dotimes (i (length data))
      (setf (gl:glaref arr i) (aref data i)))
    (gl:buffer-data target :static-draw arr)
    (gl:free-gl-array arr)))

(defmethod initialize-instance :after ((obj vx-buffer) &key data size)
  (with-slots (id) obj
    (setf id (gl:gen-buffer))
    (gl:bind-buffer :array-buffer id)
    (alloc-gl-array data size :array-buffer)))

(defun make-ix-buffer (count)
  (let ((arr (gl:alloc-gl-array :uint count)))
    (dotimes (i count)
      (setf (gl:glaref arr i) i))
    arr))

(defmethod buffer-bind ((obj vx-buffer))
  (with-slots (id) obj (gl:bind-buffer :array-buffer id)))

(defmethod buffer-unbind ((obj vx-buffer))
  (gl:bind-buffer :array-buffer 0))

(defun draw (va ib shader)
  (buffer-bind va)
  (gl:use-program shader)
  (gl:draw-elements :triangles ib))

(defun main ()
  (glfw:def-window-size-callback update-viewport (window w h)
    (declare (ignore window))
    (set-viewport w h))

  (glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
    (declare (ignore window scancode mod-keys))
    (when (and (eq key :escape) (eq action :press))
      (glfw:set-window-should-close)))
  
  (with-body-in-main-thread ()
    (glfw:with-init-window (:title "untitled" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)

      (let* ((verticies #( 0.0  0.5 0.0
			  0.5 -0.5 0.0
			  -0.5 -0.5 0.0 ))
	     (vx-buffer (make-instance 'vx-buffer :data verticies :size 12))
	     (ix-buffer (make-ix-buffer 3)))


	;; TODO: vertex-array
	(gl:vertex-attrib-pointer 0 3 :float nil 12 (cffi:null-pointer))
	(gl:enable-vertex-attrib-array 0)

	(let* ((shader (create-shader *vs-src* *fs-src*)))
	  (loop until (window-should-close-p)
		do (gl:with-pushed-matrix
		     (progn (gl:clear-color 0.07 0.13 0.17 1.0)
			    (gl:clear :color-buffer-bit :depth-buffer-bit)
			    (draw vx-buffer ix-buffer shader)
			    (swap-buffers)))
		do (poll-events))
	  (gl:delete-program shader))))))

(main)
