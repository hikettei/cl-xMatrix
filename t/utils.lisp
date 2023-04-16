

(in-package :cl-xmatrix-test)

(defun fill-with-random-negative-fp32 (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					     (- (random 1.0) 2.5))))))
