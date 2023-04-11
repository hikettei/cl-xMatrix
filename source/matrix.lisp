
(in-package :cl-xmatrix)

(defparameter *available-dtypes*
  `(:uint8
    :float
    :double))

(defun dtype-p (x)
  (if (find x *available-dtypes*)
      t
      nil))

(deftype matrix-dtype ()
  "The type of available dtypes."
  `(and keyword
	(satisfies dtype-p)))

(defun index-p (x)
  (>= x 0))

(deftype index ()
  "x >= 0"
  `(and (satisfies index-p)))

(defun allocate-mat (size &key (dtype :float))
  (declare (type matrix-dtype dtype)
	   (type index size))
  (foreign-alloc
   dtype
   :count size))

(defun free-mat (matrix)
  "delete matrix"
  (declare (type matrix matrix))c
  (foreign-free (matrix-vec matrix)))


(declaim (ftype (function (cons fixnum) cons) fill-with-d))
(defun fill-with-d (shape i)
  (declare (optimize (speed 3))
	   (type cons shape)
	   (type fixnum i))
  (let ((index -1))
    (declare (type fixnum index))
    (map 'list (lambda (x)
		 (declare (ignore x))
		 (incf index 1)
		 (cond
		   ((= i index)
		    1)
		   (T 0)))
	 shape)))

(defun get-stride (shape dim)
  (let ((subscripts (fill-with-d shape dim)))
    (apply #'+ (maplist #'(lambda (x y)
			    (the fixnum
				 (* (the fixnum (car x))
				    (the fixnum (apply #'* (cdr y))))))
			subscripts
			shape))))

(defun calc-strides (shapes)
  (map 'list #'(lambda (x) (get-stride shapes x)) shapes))

(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  (format stream "<Matrix :~(~a~) :shape ~a :visible-area ~a~% :vec ~a>"
	  (matrix-dtype matrix)
	  (matrix-shape matrix)
	  (matrix-view matrix)
	  (matrix-vec matrix))) ; TODO: more infos

(defstruct (Matrix
	    (:print-function print-matrix)
	    (:constructor
		matrix (shape &key (dtype :float) &aux (view `(t))))
	    (:constructor
		view-of-matrix (matrix &rest view
				&aux
				  (shape (matrix-shape matrix))
				  (dtype (matrix-dtype matrix)))))
  (vec (allocate-mat (apply #'* shape) :dtype dtype))
  (dtype dtype :type matrix-dtype)
  (shape shape :type cons)
  (view view :type cons)
  (strides (calc-strides shape) :type cons))



