
(in-package :cl-xmatrix)

;; 
;;
;;


(macrolet ((define-elwise-cfun (name dtype)
	     `(defcfun ,name :void
		(view (:struct ViewInstruction))
		(array (:pointer ,dtype)))))  
  (define-elwise-cfun "fp32_abs" :float))

(defun %abs (matrix)
  "Computes absolute values destructively."
  (call-with-visible-area
   matrix
   #'(lambda (view)
       (fp32-abs view (matrix-vec matrix)))))

(defun %square (matrix)
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
