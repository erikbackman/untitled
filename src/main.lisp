(in-package :untitled)

(defparameter *win-w* 800)
(defparameter *win-h* 600)
(defparameter *aspect* (/ *win-w* *win-h*))

(defun set-viewport (width height)
  (setf *win-w* width
	*win-h* height
	*aspect* (/ width height))
  (gl:viewport 0 0 width height))

(defun init-gl ()
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1.0)
  (gl:matrix-mode :modelview)
  (gl:enable :depth-test)
  (gl:enable :multisample)
  ;; (gl:polygon-mode :front-and-back :line)
  (gl:depth-func :lequal)
  (gl:hint :perspective-correction-hint :nicest)
  )

;; TODO: camera abstraction
(defparameter *rotation* #(0.0 0.0))
(defparameter *zoom* 0.0)

(defun inc-rotation (dx dy)
  (incf (aref *rotation* 0) (* 2 dx))
  (incf (aref *rotation* 1) (* 2 dy)))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))       
  (set-viewport w h))

(def-key-callback handle-key-input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq action :press) (eq key :escape))
    (glfw:set-window-should-close))
  (when (or (eq action :press)
	    (eq action :repeat))
    (case key
      (:s (inc-rotation -1 +0))
      (:w (inc-rotation +1 +0))
      (:a (inc-rotation +0 -1))
      (:d (inc-rotation +0 +1))
      (:q (incf *zoom* -0.01))
      (:e (incf *zoom* +0.01)))))

(def-scroll-callback zoom-on-scroll (window x-offset y-offset)
       (declare (ignore window x-offset))
       (if (plusp y-offset) (incf *zoom* 0.01)
	   (decf *zoom* 0.01)))

(defmacro with-window ((&key title width height) &body body)
  `(with-body-in-main-thread ()     
     (with-init-window (:title ,title
			:width ,width :height ,height
			:samples 4 :refresh-rate 60)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       (set-key-callback 'handle-key-input)
       (set-scroll-callback 'zoom-on-scroll)
       (set-window-size-callback 'update-viewport)
       (glfw:swap-interval 1)		; vsync
       (init-gl)
       (set-viewport ,width ,height)
       ,@body)))

(defparameter *fdelay* (/ 1.0 60.0))

(defun main ()
  (with-window (:title "untitled" :width *win-w* :height *win-h*)
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
