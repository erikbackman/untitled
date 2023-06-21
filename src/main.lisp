(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-glfw3")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria")
  (ql:quickload "trivial-main-thread"))

(defpackage untitled
  (:use :cl :glfw :trivial-main-thread :cl-opengl))

(in-package :untitled)

(glfw:def-key-callback quit-on-escape (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:with-pushed-matrix
    (gl:color 1 1 1)
    (gl:rect -25 -25 25 25)))

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

(defun basic-window-example ()
  (with-body-in-main-thread ()
    (glfw:with-init-window (:title "Window test" :width 600 :height 400)
      (setf %gl:*gl-get-proc-address* #'get-proc-address)
      (set-key-callback 'quit-on-escape)
      (set-window-size-callback 'update-viewport)
      (gl:clear-color 0 0 0 0)
      (set-viewport 600 400)
      (loop until (window-should-close-p)
            do (render)
            do (swap-buffers)
            do (poll-events)))))

(basic-window-example)
