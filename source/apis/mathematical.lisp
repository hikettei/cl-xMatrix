
(in-package :cl-xmatrix)

;; 
;;
;;

(defcfun "fp32_abs" :void
	      (view (:struct ViewInstruction))
	      (array (:pointer :float)))

(defun %abs (matrix)
  "Computes absolute values destructively."
  (call-with-visible-area
   matrix
   #'(lambda (view)
       (fp32-abs view (matrix-vec matrix)))))

