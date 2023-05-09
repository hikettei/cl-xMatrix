
(in-package :cl-xmatrix)

;; Here's primitive operations (exported in another names)

;; To Add: arange a[i] = alpha*i + beta. i = (k, m)

;(define-method-combination static-shape ())

;; https://eshamster.hatenablog.com/entry/play-define-method-combination-01

;; Increment -> bordeaux-threads/?

(macrolet ((define-arithmetic-cfun (name dtype)
	     `(defcfun ,name :void
		(view1 :pointer)
		(view2 :pointer)
		(array1 (:pointer ,dtype))
		(array2 (:pointer ,dtype)))))
  
  (define-arithmetic-cfun "fp32_add" :float)
  (define-arithmetic-cfun "fp32_sub" :float)
  (define-arithmetic-cfun "fp32_mul" :float)
  (define-arithmetic-cfun "fp32_div" :float)

  (define-arithmetic-cfun "fp16_add" :uint16)
  (define-arithmetic-cfun "fp16_sub" :uint16)
  (define-arithmetic-cfun "fp16_mul" :uint16)
  (define-arithmetic-cfun "fp16_div" :uint16)
  
  (define-arithmetic-cfun "fp8_add" :uint8)
  (define-arithmetic-cfun "fp8_sub" :uint8)
  (define-arithmetic-cfun "fp8_mul" :uint8)
  (define-arithmetic-cfun "fp8_div" :uint8)

  (define-arithmetic-cfun "int_add" :int)
  (define-arithmetic-cfun "int_sub" :int)
  (define-arithmetic-cfun "int_mul" :int)
  (define-arithmetic-cfun "int_div" :int)
  
  (define-arithmetic-cfun "fp32_copy" :float)
  (define-arithmetic-cfun "fp16_copy" :uint16)
  (define-arithmetic-cfun "fp8_copy"  :uint8)
  (define-arithmetic-cfun "int_copy"  :int))


(declaim (ftype (function (matrix matrix) matrix)
		%adds
		%subs
		%muls
		%divs
		%move))
(defun %adds (matrix matrix1)
  "The function %adds adds matrix1 into matrix.

where:
  - matrix and matrix1 both has the same shape, and dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1)
	   (inline call-with-visible-area))

  (assert-dtype matrix matrix1)
  (assure-dimensions matrix matrix1)

  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-add x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-add x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-add x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-add x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix)


(defun %subs (matrix matrix1)
  "The function %subs substracts matrix by matrix1.

where:
  - matrix and matrix1 both has the same shape, and dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  (assure-dimensions matrix matrix1)
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-sub x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-sub x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-sub  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-sub  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix)


(defun %muls (matrix matrix1)
  "The function %muls multiplies matrix and matrix1 element by element.

where:
    - matrix and matrix1 both has the same shape, and dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  (assure-dimensions matrix matrix1)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-mul x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-mul x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-mul  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-mul  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix)


(defun %divs (matrix matrix1)
  "The function %divs divides matrix by matrix1 element by element.

where:
    - matrix and matrix1 both has the same shape, and dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  (assure-dimensions matrix matrix1)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-div x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-div x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-div  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-div  x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix)


(defun %move (matrix matrix1)
  "The function %move copies matrix's visible area into matrix1's visible area.

where:
    - matrix and matrix1 both has the same shape, and dtype.

SideEffects:
  - matrix1 will be modified.

Return:
  - copied matrix1."
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  (assure-dimensions matrix matrix1)
  
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-copy x-view x1-view  (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-copy x-view x1-view  (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix1)


(defun %copy (matrix)
  "The function %copy allocates the new matrix whose shape is the equivalent to matrix's visible shape, and moves the corresponding elements into the new matrix.

where:
    - matrix is a matrix.

SideEffects:
  - No.

Return:
  - The copied matrix (whose shape is the same as the matrix's visible shape)"
  (declare (optimize (speed 3))
	   (type matrix matrix))
  (let ((new-matrix (matrix (matrix-visible-shape matrix)
			    :dtype (matrix-dtype matrix))))
    (%move matrix new-matrix)
    new-matrix))

(macrolet ((define-scalar-mat-cfun (name dtype)
	     `(defcfun ,name :void
		(view :pointer)
		(array (:pointer ,dtype))
		(scalar ,dtype))))
  (define-scalar-mat-cfun "fp32_scalar_add" :float)
  (define-scalar-mat-cfun "fp16_scalar_add" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_add" :uint8)
  (define-scalar-mat-cfun "int_scalar_add" :int)

  (define-scalar-mat-cfun "fp32_scalar_sub" :float)
  (define-scalar-mat-cfun "fp16_scalar_sub" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_sub" :uint8)
  (define-scalar-mat-cfun "int_scalar_sub" :int)

  (define-scalar-mat-cfun "fp32_scalar_mul" :float)
  (define-scalar-mat-cfun "fp16_scalar_mul" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_mul" :uint8)
  (define-scalar-mat-cfun "int_scalar_mul" :int)

  (define-scalar-mat-cfun "fp32_scalar_div" :float)
  (define-scalar-mat-cfun "fp16_scalar_div" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_div" :uint8)
  (define-scalar-mat-cfun "int_scalar_div" :int)

  (define-scalar-mat-cfun "fp32_fill" :float)
  (define-scalar-mat-cfun "fp16_fill" :uint16)
  (define-scalar-mat-cfun "fp8_fill" :uint8)
  (define-scalar-mat-cfun "int_fill" :int))

(defun %scalar-add (matrix scalar
		    &aux (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
  "The function %scalar-add adds the given scalar value into the matrix.

Note: scalar is coerced into the matrix's dtype (This could be lead to the performance problem. FIXME)

where:
    - matrix and scalar has the same dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3) (safety 0)))
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-add x (matrix-vec matrix) scalar))
	 (:uint16
	  (fp16-scalar-add x (matrix-vec matrix) scalar))
	 (:uint8
	  (fp8-scalar-add x (matrix-vec matrix) scalar))
	 (:int
	  (int-scalar-add x (matrix-vec matrix) scalar)))))
  matrix)

(defun %scalar-sub (matrix scalar &aux (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
    "The function %scalar-sub substracts the matrix by the given scalar..

Note: scalar is coerced into the matrix's dtype (This could be lead to the performance problem. FIXME)

where:
    - matrix and scalar has the same dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3) (safety 0)))
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-sub x (matrix-vec matrix) scalar))
	 (:uint16
	  (fp16-scalar-sub x (matrix-vec matrix) scalar))
	 (:uint8
	  (fp8-scalar-sub x (matrix-vec matrix) scalar))
	 (:int
	  (int-scalar-sub x (matrix-vec matrix) scalar)))))
  matrix)


(defun %scalar-mul (matrix scalar &aux (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
    "The function %scalar-mul multiplies the given scalar value and the matrix.

Note: scalar is coerced into the matrix's dtype (This could be lead to the performance problem. FIXME)

where:
    - matrix and scalar has the same dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3) (safety 0)))
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-mul x (matrix-vec matrix) scalar))
	 (:uint16
	  (fp16-scalar-mul x (matrix-vec matrix) scalar))
	 (:uint8
	  (fp8-scalar-mul x (matrix-vec matrix) scalar))
	 (:int
	  (int-scalar-mul x (matrix-vec matrix) scalar)))))
  matrix)

(defun %scalar-div (matrix scalar &aux (scalar (coerce scalar (dtype->lisp-type (matrix-dtype matrix)))))
    "The function %scalar-div divides the matrix by scalar.

Note: scalar is coerced into the matrix's dtype (This could be lead to the performance problem. FIXME)

where:
    - matrix and scalar has the same dtype.

SideEffects:
  - matrix will be modified.

Return:
  - modified matrix."
  (declare (optimize (speed 3) (safety 0)))
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x)
       (dtypecase matrix
	 (:float
	  (fp32-scalar-div x (matrix-vec matrix) scalar))
	 (:uint16
	  (fp16-scalar-div x (matrix-vec matrix) scalar))
	 (:uint8
	  (fp8-scalar-div x (matrix-vec matrix) scalar))
	 (:int
	  (int-scalar-div x (matrix-vec matrix) scalar)))))
  matrix)


(defun %fill (matrix scalar)
  "The function %fill fills all the matrix's visible area with scalar value.

where:
    - scalar and matrix has the same dtype.

SideEffects:
    - matrix will be modified.

Return:
    - modified matrix."
  (declare (optimize (speed 3)))
  (call-with-facet-and-visible-area
   matrix
   :foreign
   #'(lambda (x)
       (dtypecase matrix
	 (:float
	  (fp32-fill x (matrix-vec matrix) scalar))
	 (:uint16
	  (fp16-fill x (matrix-vec matrix) scalar))
	 (:uint8
	  (fp8-fill x (matrix-vec matrix) scalar))
	 (:int
	  (int-fill x (matrix-vec matrix) scalar)))))
  matrix)

