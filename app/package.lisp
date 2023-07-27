#+sbcl
(when (<= (floor (sb-ext:dynamic-space-size) (* 1024 1024 1024)) 1)
  (error "Use sbcl --dynamic-space-size 4GB"))

(defpackage :untitled
  (:nicknames :ut)
  (:use :cl :trivial-main-thread)
  (:local-nicknames (:cg :sb-cga))
  (:import-from :cl-glfw3 :set-window-size-callback :set-key-callback)
  (:import-from :g3
   :with-window :poll-events :renderer-reset-stats
   :renderer-init :renderer-begin-scene :renderer-set-clear-color
   :render-batch :handle-key-input :handle-mouse-movement
   :draw-quad-rotated :draw-cube :draw-plane-points :draw-line
   :renderer-flush :swap-buffers :shutdown
   :camera-handle-keyboard :*camera* :*timestep*
   :draw-sphere)
;;  (:import-from :cl-opengl)
  (:import-from :alexandria :switch :with-gensyms)
  (:import-from :trivia :match :defpattern :guard1)
;;  (:import-from :gl :clear)
  (:import-from :sb-cga :transform-point :translate :vec :matrix*)
;;  (:shadow :with-window :create-shader)
  )
