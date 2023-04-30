
(in-package :cl-xmatrix-test)

(in-suite :test)

;; element-wise 1 argument functions.

(macrolet ((define-1dfunc-tester (name func-name lisp-func)
	     `(progn
		(defun ,name (dtype)
		  (let* ((mat1 (matrix `(10 10) :dtype dtype))
			 (mat2 (%copy mat1)))
		    (with-pointer-barricade
		      (funcall #',func-name mat1)
		      (%all?
		       (%compare mat1 mat2 #'(lambda (x y) (= x (,lisp-func y))))))))
		(test ,name
		  (is (,name :float))
		  (is (,name :uint16))
		  (is (,name :uint8))
		  (is (,name :int))))))
  (define-1dfunc-tester test-abs %abs abs)
  (define-1dfunc-tester test-log %log log)
  (define-1dfunc-tester test-log2 %log2 (lambda (x) (log x 2)))
  (define-1dfunc-tester test-log10 %log10 (lambda (x) (log x 10)))

  (define-1dfunc-tester test-exp %exp exp)

  (define-1dfunc-tester test-sqrt %sqrt sqrt)
  
  (define-1dfunc-tester test-sin %sin sin)
  (define-1dfunc-tester test-cos %cos cos)
  (define-1dfunc-tester test-tan %tan tan)

  
  (define-1dfunc-tester test-asin %asin asin)
  (define-1dfunc-tester test-acos %acos acos)
  (define-1dfunc-tester test-atan %atan atan)

  (define-1dfunc-tester test-sinh %sinh sinh)
  (define-1dfunc-tester test-cosh %cosh cosh)
  (define-1dfunc-tester test-tanh %tanh tanh)

  (define-1dfunc-tester test-asinh %asinh asinh)
  (define-1dfunc-tester test-acosh %acosh acosh)
  (define-1dfunc-tester test-atanh %atanh atanh))
