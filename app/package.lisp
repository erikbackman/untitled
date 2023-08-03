#+sbcl
(when (<= (floor (sb-ext:dynamic-space-size) (* 1024 1024 1024)) 1)
  (error "Use sbcl --dynamic-space-size 4GB"))

(defpackage :untitled
  (:nicknames :ut)
  (:use #:cl #:trivial-main-thread)
  (:local-nicknames (:cg :sb-cga))
  (:import-from #:cl-glfw3
   :set-window-size-callback
   :set-key-callback)
  (:import-from #:g3d
   :with-window
   :poll-events
   :renderer-reset-stats
   :renderer-init
   :renderer-begin-scene
   :renderer-set-clear-color
   :handle-key-input
   :handle-mouse-movement
   :draw-quad-rotated
   :draw-cube
   :draw-plane-points
   :draw-plane-normal
   :draw-line
   :swap-buffers
   :shutdown
   :renderer-present
   :camera-handle-keyboard
   :*camera*
   :*timestep*
   :draw-sphere
   :renderer
   :shader-set-mat4
   :shader-get-uniform
   :*renderer*
   :renderer-get-shader
   :sphere-shader
   :quad-shader
   :get-current-scene
   :with-scene
   :scene-submit
   :scene-add
   :scene-set
   :line
   :cube
   :sphere
   :plane
   :plane%
   :line%
   :geometry)
  (:import-from #:alexandria
   :switch
   :with-gensyms)
  (:import-from #:trivia
   :match
   :defpattern
   :guard1)
  (:import-from :sb-cga :transform-point :translate :vec :matrix*))
