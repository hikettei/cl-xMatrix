
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.least-squares
  (:use :cl :cl-waffe)
  (:export #:optimize-with-ridge-regression))

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
		 (backward loss)
		 (update)
		 loss))
  :predict ((x) (call (model) x)))

(defun optimize-with-ridge-regression (x-binary y proto-shape
				       &key
					 (alpha 1.0)
					 (lr 1e-2)
					 (required-loss 1.0)
				       &aux
					 (input  (car (cl-xmatrix:shape y)))
					 (output (second (cl-xmatrix:shape y)))
					 (D (/ (length x-binary) input)))
  "Minimizes The Function ||y - Xw||^2_2 + alpha * ||w||^2_2

Inputs:
  x-binary = [N, C*K] (simple-array)
  y        = [N C]    (matrix)"
  (declare (type simple-array x-binary)
	   (type single-float alpha lr)
	   (type cl-xmatrix:matrix y))

  (with-dtype :float
    (let ((model  (RidgeModel output D alpha lr))
	  (sparse (mgl-mat:array-to-mat x-binary))
	  (inputs (mgl-mat:array-to-mat
		   (cl-xmatrix:convert-into-lisp-array y))))
      (let ((loss (+ 0.1 required-loss)))
	(loop while (< loss required-loss)
	      do (setq loss (data
			     (step-model
			      model
			      (!reshape (const inputs) `(,input t))
			      (!reshape (const sparse) `(,input t)))))))
      ;; weight [D, C * K]
      (let ((coeff (slot-value
		    (slot-value
		     (slot-value model 'model)
		     'linear)
		    'cl-waffe.nn::weight)))

	;; linear = weight @ x.
	;; is transpose needed?
	(!reshape coeff proto-shape)))))
