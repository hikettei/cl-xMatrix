
(in-package :cl-xmatrix)

;; 
;;
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    "concatenates args by printing into string"
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    "interns the mkstr output/returns as symbol"
    (values (intern (apply #'mkstr args)))))

;; Maximize/Minimize
(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((define-elwise-cfun (name dtype)
	       `(defcfun ,name :void
		  (view (:struct ViewInstruction))
		  (array (:pointer ,dtype)))))
    
    (define-elwise-cfun "fp32_abs" :float)
    (define-elwise-cfun "fp16_abs" :uint16)
    (define-elwise-cfun "fp8_abs" :uint8)
    (define-elwise-cfun "int_abs" :int)

    (define-elwise-cfun "fp32_sin" :float)
    (define-elwise-cfun "fp16_sin" :uint16)
    (define-elwise-cfun "fp8_sin"  :uint8)
    (define-elwise-cfun "int_sin"  :int)

    (define-elwise-cfun "fp32_cos" :float)
    (define-elwise-cfun "fp16_cos" :uint16)
    (define-elwise-cfun "fp8_cos"  :uint8)
    (define-elwise-cfun "int_cos"  :int)
    
    (define-elwise-cfun "fp32_tan" :float)
    (define-elwise-cfun "fp16_tan" :uint16)
    (define-elwise-cfun "fp8_tan"  :uint8)
    (define-elwise-cfun "int_tan"  :int)

    (define-elwise-cfun "fp32_asin" :float)
    (define-elwise-cfun "fp16_asin" :uint16)
    (define-elwise-cfun "fp8_asin"  :uint8)
    (define-elwise-cfun "int_asin"  :int)

    (define-elwise-cfun "fp32_acos" :float)
    (define-elwise-cfun "fp16_acos" :uint16)
    (define-elwise-cfun "fp8_acos"  :uint8)
    (define-elwise-cfun "int_acos"  :int)
    
    (define-elwise-cfun "fp32_atan" :float)
    (define-elwise-cfun "fp16_atan" :uint16)
    (define-elwise-cfun "fp8_atan"  :uint8)
    (define-elwise-cfun "int_atan"  :int)
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((define-elwise-1dfunc (fname docstring name)
	       `(defun ,fname (matrix)
		  ,docstring
		  (declare (optimize (speed 3) (safety 0))
			   (type matrix matrix))
		  (call-with-visible-area
		   matrix
		   #'(lambda (view)
		       (dtypecase matrix
			 (:float
			  (funcall #',(symb 'fp32- name) view (matrix-vec matrix)))
			 (:uint16
			  (funcall #',(symb 'fp16- name) view (matrix-vec matrix)))
			 (:uint8
			  (funcall #',(symb 'fp8- name) view (matrix-vec matrix)))
			 (:int
			  (funcall #',(symb 'int- name) view (matrix-vec matrix))))))
		  matrix)))

    (define-elwise-1dfunc %abs
      "Computes absolute values."
      abs)

    (define-elwise-1dfunc %sin
      ""
      sin)

    (define-elwise-1dfunc %cos
      ""
      cos)

    (define-elwise-1dfunc %tan
      ""
      tan)
    
    
    ))


(defun %square (matrix)
  ""
  (%muls matrix matrix)
  matrix)

;; sin cos tan sinh cosh tanh asin acos atan asinh acosh atanh
;; exp log
;; maximize minimize compare
;; square/expt


;; Reduction

;; sum mean

;; gemm
;; reshape squeeze unsqueeze

;; beta gamma normal etc...
