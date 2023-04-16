
(in-package :cl-user)

(defpackage cl-xmatrix-test
  (:use :cl :uiop :asdf :cffi :cl-xmatrix)
  (:import-from
   :fiveam
   :def-suite
   :in-suite
   :test
   :is))

(in-package :cl-xmatrix-test)

(def-suite :test)

(in-suite :test)


(cl-xmatrix::load-xmatrix)

(defun test-mithral-learn ()
  (let ((a (cl-xmatrix::matrix `(128 128))))
    (cl-xmatrix::randn a)
    (cl-xmatrix::init-and-learn-mithral a 16 4)
    (setq a (cl-xmatrix::matrix `(8 128)))
    (cl-xmatrix::compute-optimal-split-val a 0)
    t))

(test hoge
  (is (= 1 1))
  (is (test-mithral-learn)))

