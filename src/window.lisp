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
  (gl:enable :multisample :depth-test)
  (gl:cull-face :back)
  (gl:front-face :cw)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:disable :cull-face) ;; disable for now
  (gl:hint :perspective-correction-hint :nicest))

(defun update-camera (key cam)
  (let ((front (camera-front cam))
	(pos (camera-pos cam))
	(speed (camera-speed cam)))
    (case key
      (:w (vec+= pos (vec3* speed front)))
      (:s (vec-= pos (vec3* speed front)))
      (:a (vec-= pos (vec3* speed (vec-normalize (vec-cross front (camera-up cam))))))
      (:d (vec+= pos (vec3* speed (vec-normalize (vec-cross front (camera-up cam)))))))))

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
    (update-camera key *camera*)))

(defmacro with-window ((&key title width height) &body body)
  `(with-body-in-main-thread ()
     (with-init-window (:title ,title
			:width ,width :height ,height
			:samples 4 :refresh-rate 60)
       (setf %gl:*gl-get-proc-address* #'get-proc-address)
       (set-window-size-callback 'update-viewport)
       (glfw:swap-interval 1)		; vsync
       (init-camera)
       (init-gl)
       (set-viewport ,width ,height)
       (set-key-callback 'handle-key-input)
       ,@body)))
