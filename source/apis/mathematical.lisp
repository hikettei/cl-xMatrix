
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
		  (view :pointer)
		  (array (:pointer ,dtype)))))
    
    (define-elwise-cfun "fp32_abs" :float)
    (define-elwise-cfun "fp16_abs" :uint16)
    (define-elwise-cfun "fp8_abs" :uint8)
    (define-elwise-cfun "int_abs" :int)
    

    (define-elwise-cfun "fp32_log" :float)
    (define-elwise-cfun "fp16_log" :uint16)
    (define-elwise-cfun "fp8_log" :uint8)
    (define-elwise-cfun "int_log" :int)
    

    (define-elwise-cfun "fp32_log2" :float)
    (define-elwise-cfun "fp16_log2" :uint16)
    (define-elwise-cfun "fp8_log2" :uint8)
    (define-elwise-cfun "int_log2" :int)
    

    (define-elwise-cfun "fp32_log10" :float)
    (define-elwise-cfun "fp16_log10" :uint16)
    (define-elwise-cfun "fp8_log10" :uint8)
    (define-elwise-cfun "int_log10" :int)
    

    (define-elwise-cfun "fp32_exp" :float)
    (define-elwise-cfun "fp16_exp" :uint16)
    (define-elwise-cfun "fp8_exp" :uint8)
    (define-elwise-cfun "int_exp" :int)


    (define-elwise-cfun "fp32_sqrt" :float)
    (define-elwise-cfun "fp16_sqrt" :uint16)
    (define-elwise-cfun "fp8_sqrt" :uint8)
    (define-elwise-cfun "int_sqrt" :int)


    (define-elwise-cfun "fp32_cbrt" :float)
    (define-elwise-cfun "fp16_cbrt" :uint16)
    (define-elwise-cfun "fp8_cbrt" :uint8)
    (define-elwise-cfun "int_cbrt" :int)
    

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


    (define-elwise-cfun "fp32_sinh" :float)
    (define-elwise-cfun "fp16_sinh" :uint16)
    (define-elwise-cfun "fp8_sinh"  :uint8)
    (define-elwise-cfun "int_sinh"  :int)
    

    (define-elwise-cfun "fp32_cosh" :float)
    (define-elwise-cfun "fp16_cosh" :uint16)
    (define-elwise-cfun "fp8_cosh"  :uint8)
    (define-elwise-cfun "int_cosh"  :int)
    
    
    (define-elwise-cfun "fp32_tanh" :float)
    (define-elwise-cfun "fp16_tanh" :uint16)
    (define-elwise-cfun "fp8_tanh"  :uint8)
    (define-elwise-cfun "int_tanh"  :int)
    

    
    (define-elwise-cfun "fp32_asinh" :float)
    (define-elwise-cfun "fp16_asinh" :uint16)
    (define-elwise-cfun "fp8_asinh"  :uint8)
    (define-elwise-cfun "int_asinh"  :int)
    

    (define-elwise-cfun "fp32_acosh" :float)
    (define-elwise-cfun "fp16_acosh" :uint16)
    (define-elwise-cfun "fp8_acosh"  :uint8)
    (define-elwise-cfun "int_acosh"  :int)
    
    
    (define-elwise-cfun "fp32_atanh" :float)
    (define-elwise-cfun "fp16_atanh" :uint16)
    (define-elwise-cfun "fp8_atanh"  :uint8)
    (define-elwise-cfun "int_atanh"  :int)
    ))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((define-elwise-1dfunc (fname docstring name)
	       `(progn
		  (export ',fname)
		  (defun ,fname (matrix)
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
			    (funcall #',(symb 'int- name) view (matrix-vec matrix)))))
		     :direction :foreign)
		    matrix))))

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


    (define-elwise-1dfunc %asin
      ""
      asin)

    (define-elwise-1dfunc %acos
      ""
      acos)

    (define-elwise-1dfunc %atan
      ""
      atan)


    (define-elwise-1dfunc %sinh
      ""
      asinh)

    (define-elwise-1dfunc %cosh
      ""
      cosh)

    (define-elwise-1dfunc %tanh
      ""
      tanh)


    (define-elwise-1dfunc %asinh
      ""
      asinh)

    (define-elwise-1dfunc %acosh
      ""
      acosh)

    (define-elwise-1dfunc %atanh
      ""
      atanh)


    (define-elwise-1dfunc %log
      ""
      log)
    
    (define-elwise-1dfunc %log2
      ""
      log2)

    (define-elwise-1dfunc %log10
      ""
      log10)

    (define-elwise-1dfunc %exp
      ""
      exp)

    (define-elwise-1dfunc %sqrt
      ""
      sqrt)
    
    (define-elwise-1dfunc %cbrt
      ""
      cbrt)
    
    
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
