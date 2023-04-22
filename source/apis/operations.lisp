
(in-package :cl-xmatrix)

;; Filter
;; Satisfies
;; Where
;; SumUp in SIMD
;; with-copying

(defun %sumup (matrix)
  ;; tmp
  (let ((total 0.0))
    (declare (optimize (safety 0)))
    (call-with-visible-area
     matrix #'(lambda (x)
		(with-view-object (index x)
		  (incf total
		   (mem-aref
		    (matrix-vec matrix) (matrix-dtype matrix) index)))))
    total))

(defun %sum (matrix &key (axis 0))
  "Sum for tmp definition (TODO: USE SIMD IN C)"
  (declare (type matrix matrix)
	   (type index axis))
;	   (optimize (speed 3)))
  (let ((shape (copy-list (matrix-visible-shape matrix))))
    (setf (nth axis shape) 1)
    (let ((matrix1 (matrix shape :dtype (matrix-dtype matrix))))
      (call-with-visible-area
       matrix
       #'(lambda (view)
	   (case axis
	     (0 ;; (M N) -> (M 1)
	      (with-slots ((offsets offsets)
			   (stride2 stride2)
			   (stride1 stride1)
			   (offset2 offset2)
			   (offset1 offset1)
			   (m m)
			   (n n))
		  view
		(dotimes (row m)
		  (with-views ((m* matrix row t)
			       (m1* matrix1 row t))
		    (setf (mem-aref (matrix-vec m1*) (matrix-dtype matrix) row)
			  (%sumup m*))))))
	     (1
	      (with-slots ((offsets offsets)
			   (stride2 stride2)
			   (stride1 stride1)
			   (offset2 offset2)
			   (offset1 offset1)
			   (m m)
			   (n n))
		  view
		(dotimes (cols n)
		  (with-views ((m* matrix t cols)
			       (m1* matrix1 t cols))
		    (setf (mem-aref (matrix-vec m1*) (matrix-dtype matrix) cols)
			  (%sumup m*))))))
	     (T (error "no impl")))))
      matrix1)))

(defun 1d-mat-aref (matrix index)
  ""
  (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index))

(defun (setf 1d-mat-aref) (value matrix index)
  "To Add: TypeCheck"
  (setf (mem-aref (matrix-vec matrix) (matrix-dtype matrix) index) value))

;; (defun add (), alias for %scalar-add %broadcast-add %adds, +
;; m+= instead of add is more intuitive naming?

(defun %filter (matrix function)
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function))
  (call-with-visible-area
   matrix
   #'(lambda (view)
       (with-view-object (i view)
	 (setf (1d-mat-aref matrix i) (funcall function (1d-mat-aref matrix i))))))
  matrix)

(defun %satisfies (matrix function)
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function))
  (let ((result (matrix (shape matrix) :dtype (dtype matrix)))
	(true-i  (coerce-to-mat-dtype 1 matrix))
	(false-i (coerce-to-mat-dtype 0 matrix)))
    (call-with-visible-area
     matrix
     #'(lambda (view)
	 (with-view-object (i view :absolute ri)
	   (setf (1d-mat-aref result ri)
		 (if (funcall function (1d-mat-aref matrix i))
		     true-i
		     false-i)))))
    result))

(defun %compare (matrix matrix1 function)
  "ex: (%compare a b #'<)"
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix matrix1)
	   (type function function))
  (let ((result (matrix (shape matrix) :dtype (dtype matrix)))
	(true-i  (coerce-to-mat-dtype 1 matrix))
	(false-i (coerce-to-mat-dtype 0 matrix)))
    (call-with-visible-area
     matrix
     #'(lambda (view1 view2)
	 (with-two-of-views ((i view1) (k view2) :absolute ri)
	   (setf (1d-mat-aref result ri)
		 (if (funcall function (1d-mat-aref matrix i) (1d-mat-aref matrix1 k))
		     true-i
		     false-i))))
     :mat-operated-with matrix1)
    result))

(defun %all? (tf-matrix)
  (= (%sumup tf-matrix) (apply #'* (shape tf-matrix))))

(defun %or? (tf-matrix)
  (>= (%sumup tf-matrix) 1))
