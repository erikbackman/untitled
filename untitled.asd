(defsystem "untitled"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "render")
		 (:file "shader"))))
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
