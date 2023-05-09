
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


(annot:enable-annot-syntax)

(defun %sumup (matrix)
  (let ((total 0.0))
    (declare (optimize (safety 0)))
    (call-with-facet-and-visible-area
     matrix
     :foreign
     #'(lambda (x)
	 (with-view-object (index x)
	   (incf total
		 (mem-aref
		  (matrix-vec matrix) (matrix-dtype matrix) index)))))
    total))

(defun %sum (matrix &key (axis 0) (out nil))
  "Sum up matrix (write docs)
todo: check out's dimensions/error check"
  ;; Todo: for multiple axis (I've confirmed it works)
  (declare (type matrix matrix)
	   (type index axis)
	   (optimize (speed 3)))
  (let* ((shape (copy-list (matrix-visible-shape matrix)))
 	 (reduction-size (nth axis shape)))
    (setf (nth axis shape) 1)
    (let ((result (or out (matrix shape :dtype (matrix-dtype matrix))))
	  (view (loop for i fixnum upfrom 0 below (1+ axis)
		      if (= i axis)
			collect `(:broadcast ,reduction-size)
		      else
			collect t)))
      (let ((result* (apply #'view result view)))
	(%adds result* matrix)
	result))))

(declaim (inline 1d-mat-aref))
(defun 1d-mat-aref (matrix index)
  ""
  (with-facet (matrix (matrix 'backing-array))
    (aref (the (simple-array t (*)) (matrix-vec matrix)) index)))

(defun (setf 1d-mat-aref) (value matrix index)
  "To Add: TypeCheck"
  (with-facet (matrix (matrix 'backing-array))
    (setf (aref (matrix-vec matrix) index) value)))

;; (defun add (), alias for %scalar-add %broadcast-add %adds, +
;; m+= instead of add is more intuitive naming?

(defun %filter (matrix function)
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function))
  (call-with-facet-and-visible-area
   matrix
   :lisp
   #'(lambda (view)
       (with-view-object (i view)
	 (setf (1d-mat-aref matrix i) (funcall function (1d-mat-aref matrix i))))))
  matrix)


(defun %index (matrix function)
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function))
  (call-with-facet-and-visible-area
   matrix
   :lisp
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
    (call-with-facet-and-visible-area
     matrix
     :lisp
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
    (call-with-facet-and-visible-area
     matrix
     :lisp
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


(macrolet ((define-cmp-cfun (cname dtype)
	     `(defcfun ,cname :void
		(view1 :pointer)
		(view2 :pointer)
		(vec (:pointer ,dtype))
		(out (:pointer ,dtype))
		(scal ,dtype))))
  (define-cmp-cfun "fp32_scalar_greater_than" :float)
  (define-cmp-cfun "fp32_scalar_less_than" :float)
  (define-cmp-cfun "fp32_scalar_greater_than_eq" :float)
  (define-cmp-cfun "fp32_scalar_less_than_eq" :float)

  )

@export
(defun %> (matrix scalar
	   &key (out nil)
	   &aux
	     (out    (or out (matrix (shape matrix) :dtype (matrix-dtype matrix))))
	     (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
  ""
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (assert-dtype matrix out)
  (assure-dimensions matrix out)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-greater-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint16
	  (fp32-scalar-greater-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint8
	  (fp32-scalar-greater-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:int
	  (fp32-scalar-greater-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))))
   :mat-operated-with out)
  out)

@export
(defun %< (matrix scalar
	   &key (out nil)
	   &aux
	     (out    (or out (matrix (shape matrix) :dtype (matrix-dtype matrix))))
	     (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
  ""
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (assert-dtype matrix out)
  (assure-dimensions matrix out)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-less-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint16
	  (fp32-scalar-less-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint8
	  (fp32-scalar-less-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:int
	  (fp32-scalar-less-than x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))))
   :mat-operated-with out)
  out)

@export
(defun %>= (matrix scalar
	    &key (out nil)
	   &aux
	     (out    (or out (matrix (shape matrix) :dtype (matrix-dtype matrix))))
	     (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
  ""
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (assert-dtype matrix out)
  (assure-dimensions matrix out)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-greater-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint16
	  (fp32-scalar-greater-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint8
	  (fp32-scalar-greater-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:int
	  (fp32-scalar-greater-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))))
   :mat-operated-with out)
  out)


@export
(defun %<= (matrix scalar
	   &key (out nil)
	   &aux
	     (out    (or out (matrix (shape matrix) :dtype (matrix-dtype matrix))))
	     (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
  ""
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (assert-dtype matrix out)
  (assure-dimensions matrix out)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-less-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint16
	  (fp32-scalar-less-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:uint8
	  (fp32-scalar-less-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))
	 (:int
	  (fp32-scalar-less-than-eq x-view x1-view (matrix-vec matrix) (matrix-vec out) scalar))))
   :mat-operated-with out)
  out)

