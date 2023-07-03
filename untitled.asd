(defsystem "untitled"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:alexandria
	       :trivia
	       :cl-glfw3
	       :cl-opengl
	       :trivial-main-thread)
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "math")
		 (:file "shapes")
		 (:file "function")
		 (:file "render")
		 (:file "shader")
		 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "untitled/tests"))))

(defsystem "untitled/tests"
  :author ""
  :license ""
  :depends-on ("untitled"
	       "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for untitled"
  :perform (test-op (op c) (symbol-call :rove :run c)))
