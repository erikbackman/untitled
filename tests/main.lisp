(defpackage untitled/tests/main
  (:use :cl :rove)
  (:import-from :untitled :matrix*))

(in-package :untitled/tests/main)

(deftest test-matrix*
  (testing "Should perform left matrix multiplication"
    (let* ((a #2A((0 0 1)
		  (1 0 0)
		  (0 1 0)))
	   (b #2A((1 2 3)
		  (4 5 6)
		  (7 8 9)))
	   (ab (matrix* a b))
	   (c #2A((7 8 9)
		  (1 2 3)
		  (4 5 6))))
      (ok (equalp ab c)))))
