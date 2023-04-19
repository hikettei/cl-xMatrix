
(in-package :cl-xmatrix)

;; Todo: Refactor
;; Memo: https://www.lispforum.com/viewtopic.php?t=4296
;; Heap Corruptionなぜ起こる？？？
;; or: Add with-mem-barricades
(defparameter *available-dtypes*
  `(:uint16
    :float
    :int))

(defun dtype-p (x)
  (if (find x *available-dtypes*)
      t
      nil))

(defun dtype->lisp-type (dtype)
  (case dtype
    (:float
     'single-float)
    (:int 'fixnum)
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
  ;; Todo: check if matrix exists
  ;; Todo: Count total-mem-usage not to forget memfree.
  ;(foreign-free (matrix-vec matrix))
  )


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

(defun visible-shape (orig-shape view)
  "Computes the shape of visible area."
  (loop for i fixnum upfrom 0 below (length (the list orig-shape))
	collect (- (view-endindex (nth i view)
				  (nth i orig-shape))
		   (view-startindex (nth i view) (nth i orig-shape)))))

(defun compute-visible-and-broadcasted-shape (shape broadcasts &key (for-print nil))
  (if broadcasts
      (loop for s in shape
	    for b in broadcasts
	    collect (if b
			(if (eql b t)
			    (if for-print
				t
				s)
			    (if for-print
				s
				(* b s)))
			s))
      shape))

(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  ;; Considering Broadcasts: (1 10) (10 nil) -> (10 10)
  (format stream "<Matrix :~(~a~) :shape ~a :view ~a :visible-shape ~a ~% :vec ~a>"
	  (matrix-dtype matrix)
	  (matrix-shape matrix)
	  (matrix-view matrix)
	  (compute-visible-and-broadcasted-shape
	   (matrix-visible-shape matrix)
	   (matrix-broadcasts matrix)
	   :for-print t)
	  (convert-into-lisp-array matrix))) ; TODO: more infos

;; Note: view-of-matrix is NOT ALLOWED to use the view-object's information
;; Use the original matrix's SHAPE, strides and so on...

(defstruct (Matrix
	    (:print-function print-matrix)
	    (:constructor
		matrix (shape &key (dtype :float)
			&aux (view (loop repeat (length (the list shape))
					 collect t))
			  (matrix-vec (allocate-mat (apply #'* shape) :dtype dtype))
			  (broadcasts nil)
			  (projected-p nil)))
	    (:constructor
		view-of-matrix (matrix
				broadcasts
				&rest view
				&aux
				  (shape (matrix-shape matrix))
				  (dtype (matrix-dtype matrix))
				  (matrix-vec (matrix-vec matrix))
				  (projected-p t)
				  (broadcasts
				   (if broadcasts
				       broadcasts
				       (matrix-broadcasts matrix)))))
	    (:constructor
		;; Todo: Debug (for view ga ayasii)
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
					       collect t))
				   (broadcasts nil)
				   (projected-p
				    (matrix-projected-p matrix)))))
  (projected-p projected-p :type boolean) ;; Is view-object?
  (vec matrix-vec) ;; The ORIGINAL Matrix's CFFI Pointer
  (dtype dtype :type matrix-dtype)
  (shape shape :type cons) ;; The ORIGINAL Matrix's shape
  (view view :type cons) ;; view instruction
  (external-operation nil)
  (external-operation-dim nil)
  (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts) :type cons) ;; visible area's shape following viewinstruction
  (broadcasts broadcasts :type list)
  (strides (calc-strides (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts)) :type cons))

;; Accessors

(declaim (ftype (function (matrix) index) dims))
(defun dims (matrix)
  "Returns the length of matrix's dimensions."
  (declare (type matrix matrix)
	   (optimize (speed 3)))
  (length (the list (matrix-visible-shape matrix))))
  
