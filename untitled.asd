(defsystem "untitled"
  :version "0.1.0"
  :author "Erik Bäckman"
  :license "GPL-3"
  :depends-on (:alexandria
	       :trivia
	       :cl-glfw3
	       :cl-opengl
	       :sb-cga
	       :trivial-main-thread)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "array-utils")
		 (:file "math")
		 (:file "input")
		 (:file "camera")
		 (:file "function")
		 (:file "shader")
		 (:file "buffer")
		 (:file "window")
		 (:file "render")
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
