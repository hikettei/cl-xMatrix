
(in-package :cl-xmatrix)

;; Filter
;; Satisfies
;; Where
;; SumUp in SIMD
;; with-copying
;; repeats/expands

;; concatenate:
;; A = [1, 10]
;; B = [1, 10]
;; C = [1, 10]
;; D = [A, B, C , A , B , ...]


(defun %sumup (matrix)
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
  "Sum up matrix (write docs)"
  ;; Todo: for multiple axis (I've confirmed it works)
  (declare (type matrix matrix)
	   (type index axis)
	   (optimize (speed 3)))
  (let* ((shape (copy-list (matrix-visible-shape matrix)))
 	 (reduction-size (nth axis shape)))
    (setf (nth axis shape) 1)
    (let ((result (matrix shape :dtype (matrix-dtype matrix)))
	  (view (loop for i fixnum upfrom 0 below (1+ axis)
		      if (= i axis)
			collect `(:broadcast ,reduction-size)
		      else
			collect t)))
      (let ((result* (apply #'view result view)))
	(%adds result* matrix)
	result))))

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

(defun %index (matrix function)
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function))
  (call-with-visible-area
   matrix
   #'(lambda (view)
       (with-view-object (i view)
	 (setf (1d-mat-aref matrix i) (funcall function i)))))
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
  (declare ;;(optimize (speed 3))
	   (type matrix tf-matrix))
  (= (%sumup tf-matrix)
     (apply #'* (shape tf-matrix))))

(defun %or? (tf-matrix)
  (declare ;;(optimize (speed 3))
	   (type matrix tf-matrix))
  (>= (%sumup tf-matrix) 1))
