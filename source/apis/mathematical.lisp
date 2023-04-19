
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

