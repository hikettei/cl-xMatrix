
(in-package :cl-xmatrix)

(defun randn (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					(- (random 1.0) 2.5))))))

(defun randu (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					(random 1.0))))))
