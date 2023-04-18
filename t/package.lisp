
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
  (let ((a (cl-xmatrix::matrix `(256 128))))
    (cl-xmatrix::randn a)
    (sb-profile:profile "CL-XMATRIX")
    (time (cl-xmatrix::init-and-learn-mithral a 16 4))
    (sb-profile:report)
    ;;(setq a (cl-xmatrix::matrix `(8 128)))
    ;;(cl-xmatrix::randu a)
    ;;(cl-xmatrix::compute-optimal-split-val a 0)
    t))

(test-mithral-learn)

(test hoge
  (is (= 1 1))
  (is (test-mithral-learn)))

