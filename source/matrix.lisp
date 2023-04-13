
(in-package :cl-xmatrix)

(defparameter *available-dtypes*
  `(:uint8
    :float))

(defun dtype-p (x)
  (if (find x *available-dtypes*)
      t
      nil))

(defun dtype->lisp-type (dtype)
  (case dtype
    (:float
     'single-float)
    (T (error "Unknown type ~a" dtype))))

(defun coerce-to-dtype (element dtype)
  (coerce element (dtype->lisp-type dtype)))

(deftype matrix-dtype ()
  "The type of available dtypes."
  `(and keyword
	(satisfies dtype-p)))

(defun index-p (x)
  (>= x 0))

(deftype index ()
  "x >= 0"
  `(and (or fixnum)
	(satisfies index-p)))

(defun allocate-mat (size &key (dtype :float))
  (declare (type matrix-dtype dtype)
	   (type index size))
  (foreign-alloc
   dtype
   :count size
   :initial-element (coerce-to-dtype 0 dtype)))

(defun free-mat (matrix)
  "Frees matrix"
  (declare (type matrix matrix))
  ; Todo: check if matrix exists
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
  (loop for i fixnum upfrom 0 below (length shapes)
	collect (get-stride shapes i)))

(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  (format stream "~a" (convert-into-lisp-array matrix))
  (format stream "<Matrix :~(~a~) :shape ~a :visible-area ~a~% :visible-vec ~a>"
	  (matrix-dtype matrix)
	  (matrix-shape matrix)
	  (matrix-view matrix)
	  (matrix-vec matrix))) ; TODO: more infos

(defstruct (Matrix
	    (:print-function print-matrix)
	    (:constructor
		matrix (shape &key
				(dtype :float)
			&aux (view (loop repeat (length (the list shape))
					 collect t))
			  (matrix-vec (allocate-mat (apply #'* shape) :dtype dtype))))
	    (:constructor
		view-of-matrix (matrix &rest view
				&aux
				  (shape (matrix-shape matrix))
				  (dtype (matrix-dtype matrix))
				  (matrix-vec (matrix-vec matrix))))
	    (:constructor
		quantize-matrix (matrix
				 &key (quantize-into :fp16)
				 &aux (matrix-vec
				       (vec-quantize-into
					(matrix-vec matrix)
					(matrix-shape matrix)
					quantize-into))
				   (dtype (vec-dtype-quantized quantize-into))
				   (shape (matrix-shape matrix))
				   (view (loop repeat (length (the list shape))
					       collect t)))))
  (vec matrix-vec)
  (dtype dtype :type matrix-dtype)
  (shape shape :type cons)
  (view view :type cons)
  (strides (calc-strides shape) :type cons))


(defun randn (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					     (- (random 1.0) 2.5))))))
