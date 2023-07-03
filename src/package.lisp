(defpackage :untitled
  (:use :cl :cl-glfw3 :trivial-main-thread)
  (:import-from :cl-opengl)
  (:import-from :alexandria :switch :with-gensyms)
  (:import-from :trivia :match :defpattern :guard1)
  (:import-from #:gl
		#:clear)
  (:shadow :with-window :create-shader))
