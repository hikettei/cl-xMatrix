
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.least-squares
  (:use :cl :cl-waffe)
  (:export))

(in-package :cl-xmatrix.amm.least-squares)


(defmodel RidgeRegression (input-size output-size &key (alpha 0.1))
  :parameters ((linear (cl-waffe.nn:LinearLayer input-size output-size))
	       (alpha  (tensor alpha)))
  :forward ((x)
	    (call (self linear) x)))

(defmethod l2-regularization ((model RidgeRegression))
  (let ((l2-reg (tensor 0.0)))
    (let ((linear (slot-value model 'linear)))
      (with-slots ((weight cl-waffe.nn::weight)) linear
	(!mul (ridgeregression-alpha model)
	      (!add l2-reg (!sqrt (!sum (!pow weight 2)))))))))

;; TODO: Impl L2Norm
(deftrainer RidgeModel (input-size output-size alpha lr)
  :model (RidgeRegression input-size output-size :alpha alpha)
  :optimizer cl-waffe.optimizers:SGD
  :optimizer-args (:lr lr)
  :step-model ((x y)
	       (zero-grad)
	       (let* ((out  (call (model) x))
		      (loss (!add (!mean (cl-waffe.nn:mse out y))
				  (l2-regularization (model)))))
		 (print loss)
		 (backward loss)
		 (update)
		 loss))
  :predict ((x) (call (model) x)))

(defun optimize-with-ridge-regression (x-binary y)
  "Minimizes The Function ||y - Xw||^2_2 + alpha * ||w||^2_2"
  (declare (type (simple-array (unsigned-byte 32) (*)) x-binary)
	   (type cl-xmatrix::matrix y))

  
  
  )
