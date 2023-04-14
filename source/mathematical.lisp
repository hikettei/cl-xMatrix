
(in-package :cl-xmatrix)

; for test
(defcfun "fp32_abs" :void
	      (view (:struct ViewInstruction))
	      (array (:pointer :float)))

(defun %abs (m)
  (call-with-visible-area m
			  #'(lambda (view)
			      (fp32-abs view (matrix-vec m)))))

(defun %square (matrix)
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix) (matrix-dtype matrix) index)
		 (expt (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index) 2)))))
  matrix)

(defun %scalar-add (matrix scalar)
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix) (matrix-dtype matrix) index)
		 (+ (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index) scalar)))))
  matrix)

(defun %adds (matrix matrix1)
  "matrix, matrix1 assumed not to have view."
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix) (matrix-dtype matrix) index)
		 (+ (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index)
		    (mem-aref (matrix-vec matrix) (matrix-dtype matrix1) index))))))
  matrix)

(defun %scalar-mul (matrix scalar)
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix) (matrix-dtype matrix) index)
		 (* (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index) scalar)))))
  matrix)

(defun %fill (matrix scalar)
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix)
		  (matrix-dtype matrix)
		  index)
		 scalar))))
  matrix)


(defun randn (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					(- (random 1.0) 2.5))))))
