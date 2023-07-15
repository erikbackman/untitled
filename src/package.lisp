#+sbcl
(when (<= (floor (sb-ext:dynamic-space-size) (* 1024 1024 1024)) 1)
  (error "Use sbcl --dynamic-space-size 4GB"))

(defpackage :untitled
  (:use :cl :cl-glfw3 :trivial-main-thread)
  (:import-from :cl-opengl)
  (:import-from :alexandria :switch :with-gensyms)
  (:import-from :trivia :match :defpattern :guard1)
  (:import-from #:gl
		#:clear)
  (:shadow :with-window :create-shader))
