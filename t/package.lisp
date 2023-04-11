
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

(test hoge
  (is (= 1 1)))

