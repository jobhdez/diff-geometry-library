(defpackage diff-geometry/tests/main
  (:use :cl
        :diff-geometry
        :rove))
(in-package :diff-geometry/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :diff-geometry)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
