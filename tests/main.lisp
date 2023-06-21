(defpackage untitled/tests/main
  (:use :cl
        :untitled
        :rove))
(in-package :untitled/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :untitled)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
