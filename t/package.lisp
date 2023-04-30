
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

(cpu-information)

;; Assume:


;;Evaluation took:
;;  0.544 seconds of real time
;;  0.510924 seconds of total run time (0.482455 user, 0.028469 system)
;;  93.93% CPU
;;  1,255,534,214 processor cycles
;; 38,407,136 bytes consed

;; Numba: 0.00008
;; Numpy: 0.080   (Colab), Intel(R) Xeon(R) CPU @ 2.20GHz


(defun test-mithral ()
  (let ((a (matrix `(5000 16))))
   ;; (sb-profile:profile "CL-XMATRIX")
    (%filter a #'(lambda (x) (random 1.0)))
    ;; a = 5000 512
    ;;(time (cl-xmatrix.amm.maddness::cumsse-cols a))
    (time (cl-xmatrix.amm.maddness:init-and-learn-mithral a 16 4))
    ;;(sb-profile:report)
    t))


;;(test mithral-training-hash-table
;;  (is (test-mithral)))
