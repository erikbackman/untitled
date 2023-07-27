(defpackage untitled/tests/main
  (:use :cl :rove)
  (:import-from :g3d
   :mk-buffer-layout
   :matrix**
   :vec-cross
   :dot))

(in-package :untitled/tests/main)

(deftest test_mk-buffer-layout
  (testing "Should generate attributes with correct offset"
    (ok (equalp
	 (mk-buffer-layout '(:type (:float 3) :name "a_position")
			   '(:type (:float 4) :name "a_color"))
	 '(:stride 28 :elements
	   ((:name "a_position" :count 3 :type :float :offset 0)
	    (:name "a_color" :count 4 :type :float :offset 12)))))))

(deftest test_matrix**
  (testing "Should perform left matrix multiplication"
    (let* ((a #2A((0 0 1)
		  (1 0 0)
		  (0 1 0)))
	   (b #2A((1 2 3)
		  (4 5 6)
		  (7 8 9)))
	   (ab (matrix** a b))
	   (c #2A((7 8 9)
		  (1 2 3)
		  (4 5 6))))
      (ok (equalp ab c)))))

(deftest test_vec-cross
  (testing "u x v should be orthogonal to u and v"
    (let* ((u #(1 2 3))
	   (v #(4 5 6))
	   (w (vec-cross u v)))
      (ok (and (zerop (dot u w))
	       (zerop (dot v w)))))))
