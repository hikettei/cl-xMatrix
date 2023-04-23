
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

(defun test-mithral ()
  (let ((a (matrix `(128 32))))
    (%filter a #'(lambda (x) (random 1.0)))
    (cl-xmatrix.amm.maddness:init-and-learn-mithral a 16 4)))


(test-mithral)

(test mithral
  (is (test-mithral)))
