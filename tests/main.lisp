(defpackage untitled/tests/main
  (:use :cl :rove)
  (:import-from :untitled
   :matrix*
   :vec-cross
   :dot
   :add-vertex
   :make-vertex-table))

(in-package :untitled/tests/main)

(deftest test_matrix*
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

(deftest test_vec-cross
  (testing "u x v should be orthogonal to u and v"
    (let* ((u #(1 2 3))
	   (v #(4 5 6))
	   (w (vec-cross u v)))
      (ok (and (zerop (dot u w))
	       (zerop (dot v w)))))))

;; (deftest test_add-vertex
;;   (testing "Should return the correct range"
;;     (let ((h (make-vertex-table)))
;;       (let ((r1 (add-vertex h #(1.0 2.0 3.0)))
;; 	    (r2 (add-vertex h #(1.0 2.0 3.0))))
;; 	(ok (equalp r1 r2))))))
