(defsystem "g3d"
  :version "0.1.0"
  :author "Erik Bäckman"
  :license "GPL-3"
  :depends-on (:alexandria
	       :trivia
	       :cl-glfw3
	       :cl-opengl
	       :sb-cga
	       :trivial-main-thread)
  :components ((:module "lib"
                :components
                ((:file "package")
		 (:file "array-utils")
		 (:file "math")
		 (:file "input")
		 (:file "camera")
		 (:file "function")
		 (:file "shader")
		 (:file "buffer")
		 (:file "colors")
		 (:file "scene")
		 (:file "window")
		 (:file "render"))))
  :build-operation "program-op"
  :description "")

(defsystem "untitled"
  :version "0.1.0"
  :author "Erik Bäckman"
  :license "GPL-3"
  :depends-on (:g3d
	       :alexandria
	       :trivia
	       :cl-glfw3
	       :cl-opengl
	       :sb-cga
	       :trivial-main-thread)
  :components ((:module "app"
                :components
                ((:file "package")
		 (:file "main"))))
  :build-operation "program-op"
  :entry-point "untitled::main"
  :description ""
  :in-order-to ((test-op (test-op "untitled/tests"))))

(defsystem "untitled/tests"
  :author ""
  :license ""
  :depends-on (:untitled
	       :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for untitled"
  :perform (test-op (op c) (symbol-call :rove :run c)))
