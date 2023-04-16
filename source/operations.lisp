
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

(defmacro with-copy ())
(defmacro with-copies ())

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
