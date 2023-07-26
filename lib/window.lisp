(in-package :g3)

(defparameter *win-w* 800)
(defparameter *win-h* 600)
(defparameter *aspect* (/ *win-w* *win-h*))

(defun set-viewport (width height)
  (setf *win-w* width
	*win-h* height
	*aspect* (/ width height))
  (gl:viewport 0 0 width height))

(defparameter *polymode* :normal)

(defun wireframe-mode ()
  (gl:polygon-mode :front-and-back :line)
  (setf *polymode* :wire))

(defun fill-mode ()
  (gl:polygon-mode :front-and-back :fill)
  (setf *polymode* :normal))

(defun toggle-mode ()
  (if (eq *polymode* :normal) (wireframe-mode) (fill-mode)))

(defun init-gl ()
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1.0)
  (gl:matrix-mode :modelview)
  (gl:enable :multisample :depth-test)
  (gl:depth-func :lequal)
  (gl:depth-mask t)
  
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :blend)

  ;;(gl:cull-face :front)
  ;;(gl:front-face :cw)
  (gl:disable :cull-face) ;; disable for now

  (gl:line-width 3.0)
  (gl:enable :line-smooth)
  
  (gl:hint :perspective-correction-hint :nicest))

(glfw:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (setf *win-w* w
	*win-h* h)
  (set-viewport w h))

(glfw:def-key-callback handle-key-input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (eq key :escape) (glfw:set-window-should-close))
  (when (and (eq key :m) (eq action :press)) (toggle-mode))
  (case action
    (:press (set-key-state key t))
    (:release (set-key-state key nil))))

;; TODO: Clean this up
(defparameter *last-y* 0)
(defparameter *last-x* 0)
(defparameter *first-mouse* t)

(glfw:def-mouse-button-callback mouse-button-callback (window button action mod-keys)
  (declare (ignore window mod-keys))
  (case button
    (:left (set-key-state :mleft (eq action :press)))
    (:right (set-key-state :mright (eq action :press)))))

(declaim (ftype (function (sb-sys:system-area-pointer single-float single-float) nil)))
(glfw:def-cursor-pos-callback handle-mouse-movement (window x y)
  (declare (ignore window))
  (when (keydown? :mleft)
    ;; (when *first-mouse*
    ;;   (setf *last-x* x
    ;; 	    *last-y* y
    ;; 	    *first-mouse* nil))
   
   (let ((xoffset (- x *last-x*))
	 (yoffset (- *last-y* y)))

     (setf *last-x* x)
     (setf *last-y* y)
     
     (camera-handle-mouse-movement *camera* xoffset yoffset))))


(defun poll-events () (glfw:poll-events))
(defun swap-buffers () (glfw:swap-buffers))

(defmacro with-window ((&key title width height on-keyboard on-mouse) &body body)
  `(glfw:with-init-window (:title ,title
			   :width ,width :height ,height
			   :samples 4 :refresh-rate 60)
     (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

     (when ,on-keyboard (glfw:set-key-callback ,on-keyboard))
     (when ,on-mouse (glfw:set-cursor-position-callback ,on-mouse))
     (glfw:set-window-size-callback 'update-viewport)
     (glfw:set-mouse-button-callback 'mouse-button-callback)
     
     (glfw:swap-interval 1)		; vsync
     (init-camera)
     (init-gl)
     (set-viewport ,width ,height)
     ,@body))
