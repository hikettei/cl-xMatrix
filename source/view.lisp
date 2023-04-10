
(in-package :cl-xmatrix)


(defun subscript-p (subscripts)
  "Returns t if the format of subscripts are correct."
  t)

(defun subscript-compatiable (matrix subscripts)
  "Returns t if the subscripts are compatiable to matrix."
  t)

(defun view (matrix &rest subscripts)
  (declare (type (satisfies subscript-p) subscripts)
	   (type Matrix matrix))
  (apply #'view-of-matrix matrix subscripts))

(defmacro within-visible-area ((index-variable matrix) &body body)
  
  )

(defmacro with-broadcasting ((index1 matrix1) (index2 matrix2) &body body)

  )
