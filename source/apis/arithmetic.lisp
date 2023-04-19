
(in-package :cl-xmatrix)

;; Broadcasting

;; Todo: Dtypes
;;       SIMD
;;       Operators
;;       Barricades

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
  (define-arithmetic-cfun "int_div" :int))

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
	  )
	 (:uint16
	  )
	 (:uint8
	  )
	 (:int
	  )))
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
	  )
	 (:uint16
	  )
	 (:uint8
	  )
	 (:int
	  )))
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
	  )
	 (:uint16
	  )
	 (:uint8
	  )
	 (:int
	  )))
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
	  )
	 (:uint16
	  )
	 (:uint8
	  )
	 (:int
	  )))
   :mat-operated-with matrix1)
  matrix)


