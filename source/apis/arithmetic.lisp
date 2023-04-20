
(in-package :cl-xmatrix)

;; Here's primitive operations (exported in another names)

(macrolet ((define-arithmetic-cfun (name dtype)
	     `(defcfun ,name :void
		(view1 (:struct ViewInstruction))
		(view2 (:struct ViewInstruction))
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
  "Todo: DOC"
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  
  (call-with-visible-area
   matrix
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
  "Todo: DOC"
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  
  (call-with-visible-area
   matrix
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
  "Todo: DOC"
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)

  
  (call-with-visible-area
   matrix
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
  "Todo: DOCS"
  (declare (optimize (speed 3))
	   (type matrix matrix matrix1))

  (assert-dtype matrix matrix1)
  
  (call-with-visible-area
   matrix
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
  "Copy matrix"
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (assert-dtype matrix matrix1)
  
  (call-with-visible-area
   matrix
   #'(lambda (x-view x1-view)
       (dtypecase matrix
	 (:float
	  (fp32-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint16
	  (fp16-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:uint8
	  (fp8-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))
	 (:int
	  (int-copy x-view x1-view (matrix-vec matrix) (matrix-vec matrix1)))))
   :mat-operated-with matrix1)
  matrix)


(defun %copy (matrix)
  "Todo: Docs"
  (declare (optimize (speed 3))
	   (type matrix matrix))
  (let ((new-matrix (matrix (matrix-visible-shape matrix)
			    :dtype (matrix-dtype matrix))))
    (%move matrix new-matrix)
    new-matrix))

(macrolet ((define-scalar-mat-cfun (name dtype)
	     `(defcfun ,name :void
		(view (:struct ViewInstruction))
		(array (:pointer ,dtype))
		(scalar ,dtype))))
  (define-scalar-mat-cfun "fp32_scalar_add" :float)
  (define-scalar-mat-cfun "fp16_scalar_add" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_add" :uint8)
  (define-scalar-mat-cfun "int_scalar_add" :int)

  (define-scalar-mat-cfun "fp32_scalar_mul" :float)
  (define-scalar-mat-cfun "fp16_scalar_mul" :uint16)
  (define-scalar-mat-cfun "fp8_scalar_mul" :uint8)
  (define-scalar-mat-cfun "int_scalar_mul" :int)

  (define-scalar-mat-cfun "fp32_fill" :float)
  (define-scalar-mat-cfun "fp16_fill" :uint16)
  (define-scalar-mat-cfun "fp8_fill" :uint8)
  (define-scalar-mat-cfun "int_fill" :int))

(defun %scalar-add (matrix scalar)
  "Todo :Docs"
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
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

(defun %scalar-sub (matrix scalar)
  (%scalar-add matrix (- scalar)))


(defun %scalar-mul (matrix scalar)
  "Todo :Docs"
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
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

(defun %scalar-div (matrix scalar)
  (%scalar-mul matrix (/ scalar)))


(defun %fill (matrix scalar)
  "Todo :Docs"
  (declare (optimize (speed 3)))
  (call-with-visible-area
   matrix #'(lambda (x)
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

