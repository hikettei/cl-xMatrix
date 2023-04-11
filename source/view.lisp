
(in-package :cl-xmatrix)

(defun print-view (view stream depth)
  (declare (ignore depth))
  (format stream "~a" 0))

(defstruct (ViewInstruction
	    (:print-function print-view))
  "A Common Instruction for xMatrix, which realise view-function.
ViewInstruction is basically created only for 2d-matrix operation, functions must handle 3d-matrix with with-view macro, and as for 2d is this object."
  (m 1 :type index)
  (n 1 :type index))

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

(defmacro with-visible-area ((index-variable matrix) &body body)
  "Under this macro, three or more dimensions matrix are expanded into 2d, and set index-variable ViewInstruction."

  (labels ((explore-batch ()
	     `(progn

		)
	     ))


    ))

(defmacro within-2d-view ()

  )

;Todo Broadcasting is with-visible-areaをまとめて二回
(defmacro with-broadcasting ((index1 matrix1) (index2 matrix2) &body body)

  )
