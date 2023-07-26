(defpackage :g3
  (:use :cl :trivial-main-thread)
  (:local-nicknames (:cg :sb-cga))
  (:import-from :cl-glfw3)
  (:import-from :cl-opengl)
  (:import-from :alexandria :switch :with-gensyms)
  (:import-from :trivia :match :defpattern :guard1)
  (:import-from :gl :clear)
  (:import-from :sb-cga :transform-point :translate :vec :matrix*)
  (:shadow :with-window :create-shader))