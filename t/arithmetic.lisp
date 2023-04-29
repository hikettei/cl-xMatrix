
(in-package :cl-xmatrix-test)

(in-suite :test)

(defparameter *dtype* :float)

;; Note Eigen: row-major order

;; Testing Arithmetic(Matrix, Matrix)

(defun test-arithmetic (op lisp-op)
  (let ((matrix1 (matrix `(100 100) :dtype *dtype*))
	(matrix2 (matrix `(100 100) :dtype *dtype*)))
    (%fill matrix1 (coerce-to-dtype 6 *dtype*))
    (%fill matrix2 (coerce-to-dtype 2 *dtype*))
    (funcall op matrix1 matrix2)
    (%all? (%satisfies
	    matrix1
	    #'(lambda (x) (= x (funcall
				lisp-op
				(coerce-to-dtype 6 *dtype*)
				(coerce-to-dtype 2 *dtype*))))))))

(defun test-arithmetic-dtype (dtype)
  (let ((*dtype* dtype))
    (and
     (test-arithmetic #'%adds #'+)
     (test-arithmetic #'%subs #'-)
     (test-arithmetic #'%muls #'*)
     (test-arithmetic #'%divs #'/)
     (test-arithmetic #'%move #'(lambda (x y) (declare (ignore y)) x)))))


(test arithmetic-fp32
  (is (test-arithmetic-dtype :float)))

(test arithmetic-fp16
  (is (test-arithmetic-dtype :uint16)))

(test arithmetic-fp8
  (is (test-arithmetic-dtype :uint8)))

;;(test arithmetic-fp4
;;  (is (test-arithmetic-dtype :uint4)))




;; Testing Arithmetic(Matrix, Scalar)

(defun test-arithmetic-matrix-scalar (op lisp-op)
  (let ((matrix1 (matrix `(100 100) :dtype *dtype*))
	(scalar (coerce-to-dtype 2 *dtype*)))
    (%fill matrix1 (coerce-to-dtype 6 *dtype*))
    (funcall op matrix1 scalar)
    (%all? (%satisfies
	    matrix1
	    #'(lambda (x) (= x (funcall
				lisp-op
				(coerce-to-dtype 6 *dtype*)
				(coerce-to-dtype 2 *dtype*))))))))

(defun test-arithmetic-dtype-matrix-scalar (dtype)
  (let ((*dtype* dtype))
    (and
     (test-arithmetic-matrix-scalar #'%scalar-add #'+)
     (test-arithmetic-matrix-scalar #'%scalar-sub #'-) ;; 3 + (- 1) <- not unsigned-byte
     (test-arithmetic-matrix-scalar #'%scalar-mul #'*)
     (test-arithmetic-matrix-scalar #'%scalar-div #'/)
     (test-arithmetic-matrix-scalar #'%fill
				    #'(lambda (x y) (declare (ignore x)) y)))))

(test arithmetic-matrix-scalar-fp32
  (is (test-arithmetic-dtype-matrix-scalar :float)))

;; Fix the C's definition
(test arithmetic-matrix-scalar-fp16
  (is (test-arithmetic-dtype-matrix-scalar :uint16)))

(test arithmetic-matrix-scalar-fp8
  (is (test-arithmetic-dtype-matrix-scalar :uint8)))

;;(test arithmetic-matrix-scalar-fp4
;;  (is (test-arithmetic-dtype-matrix-scalar :uint4)))
