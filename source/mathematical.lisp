
(in-package :cl-xmatrix)


; for test
(defcfun "fp32_abs" :void
	      (view (:struct ViewInstruction))
	      (array (:pointer :float)))

(defun absm (m)
  (call-with-visible-area m
			  #'(lambda (view)
			      (print (fp32-abs view (matrix-vec m))))))
