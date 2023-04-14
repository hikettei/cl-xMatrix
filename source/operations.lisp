
(in-package :cl-xmatrix)

;; Copies vec1's visible area into vec2
(defcfun "fp32_copy" :void
  (view (:struct ViewInstruction))
  (vec1 (:pointer :float))
  (vec2 (:pointer :float)))

(defun %copy (matrix)
  ""
  (let ((matrix1 (matrix (matrix-visible-shape matrix)
			 :dtype (matrix-dtype matrix))))
    (call-with-visible-area matrix #'(lambda (view)
				       (fp32-copy view
						  (matrix-vec matrix)
						  (matrix-vec matrix1))))
    matrix1))
