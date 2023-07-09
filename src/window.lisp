(in-package :untitled)

(defparameter *win-w* 800)
(defparameter *win-h* 600)
(defparameter *aspect* (/ *win-w* *win-h*))

(defun set-viewport (width height)
  (setf *win-w* width
	*win-h* height
	*aspect* (/ width height))
  (gl:viewport 0 0 width height))

(defun wireframe-mode () (gl:polygon-mode :front-and-back :line))

(defun init-gl ()
  (gl:shade-model :smooth)
  (gl:clear-color 0 0 0 0)
  (gl:clear-depth 1.0)
  (gl:matrix-mode :modelview)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :multisample :depth-test :blend)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:disable :cull-face) ;; disable for now
  (gl:hint :perspective-correction-hint :nicest))

(def-window-size-callback update-viewport (window w h)
  (declare (ignore window))
  (setf *win-w* w
	*win-h* h)
  (set-viewport w h))

(def-key-callback handle-key-input (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq action :press) (eq key :escape))
    (glfw:set-window-should-close))
  (when (or (eq action :press)
	    (eq action :repeat))
    (camera-handle-keyboard key *camera*)))

;; TODO: Clean this up
(defparameter *last-y* 0)
(defparameter *last-x* 0)
(defparameter *first-mouse* t)

(def-cursor-pos-callback handle-mouse-movement (window x y)
  (declare (ignore window))

  (when *first-mouse*
    (setf *last-x* x
	  *last-y* y
	  *first-mouse* nil))
  
  (let ((xoffset (- x *last-x*))
	(yoffset (- *last-y* y)))

    (setf *last-x* x
	  *last-y* y)
    
    (camera-handle-mouse-movement *camera* xoffset yoffset)))

(defmacro with-window ((&key title width height on-keyboard on-mouse) &body body)
  `(with-body-in-main-thread ()
     (with-init-window (:title ,title
			:width ,width :height ,height
			:samples 4 :refresh-rate 60)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       
       (when ,on-keyboard (set-key-callback ,on-keyboard))
       (when ,on-mouse (set-cursor-position-callback ,on-mouse))
       (set-window-size-callback 'update-viewport)
       
       (glfw:swap-interval 1)		; vsync
       (init-camera)
       (init-gl)
       (set-viewport ,width ,height)
       ,@body)))
