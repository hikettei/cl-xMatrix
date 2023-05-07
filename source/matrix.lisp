
(in-package :cl-xmatrix)

;; Todo: Refactor
;; Memo: https://www.lispforum.com/viewtopic.php?t=4296

;; (define-mat-dtype
;;        :uint8
;;        available-transfer :retained-by ~~)
;; todo: support define-vop

(annot:enable-annot-syntax)

@export
(defparameter *available-dtypes*
  `(:uint8
    :uint16
    :float
    :int)
  "An list of available dtypes for Matrix.")

(defparameter *pinned-matrices* nil "A list of matrices, created in with-pointer-barricade")

@export
(defun dtype-p (x)
  "Returns t if the x (given as keyword) is available as dtype, otherwise nil."
  (declare (type keyword x))
  (if (find x *available-dtypes*)
      t
      nil))

@export
(defun dtype->lisp-type (dtype)
  "This function converts the given dtype keyword into a common lisp one."
  (declare (type keyword dtype))
  (case dtype
    (:float
     'single-float)
    (:int 'fixnum)
    (:uint8 '(unsigned-byte 8))  ;;'(integer -256 256))
    (:uint16 '(unsigned-byte 16)) ;;'(integer -65536 65536))
    (T (error "The given type is unknown:~a.~% Available dtype is following: ~a" dtype *available-dtypes*))))

@export
(defun coerce-to-dtype (element dtype)
  "This function coerces the given element (type of number) into dtype.

Example:
    (coerce 1 :float) ;; => 1.0"
  (coerce element (dtype->lisp-type dtype)))

@export
(defun coerce-to-mat-dtype (element matrix)
  "This functio coerces the given element into matrix's dtype."
  (coerce-to-dtype element (matrix-dtype matrix)))

@export
(deftype matrix-dtype ()
  "The type of available dtypes."
  `(and keyword
	(satisfies dtype-p)))

(declaim (ftype (function (fixnum) boolean) index-p)
	 (inline index-p))
(defun index-p (x)
  (declare (type fixnum x)
	   (optimize (speed 3) (safety 0)))
  (>= x 0))

(deftype index ()
  "x >= 0"
  `(and (or fixnum)
	(satisfies index-p)))


(eval-when (:compile-toplevel :execute :load-toplevel)
  (macrolet ((define-allocate-cfun (fname dtype)
	       `(defcfun ,fname (:pointer ,dtype)
		  (size :int))))
    (define-allocate-cfun "fp32_allocate_aligned_mat" :float)
    (define-allocate-cfun "fp16_allocate_aligned_mat" :uint16)
    (define-allocate-cfun "fp8_allocate_aligned_mat"  :uint8)
    (define-allocate-cfun "int_allocate_aligned_mat"  :int)))

(defun allocate-mat (size &key (dtype :float))
  (declare (type matrix-dtype dtype)
	   (type index size))
  (let ((result
	  (foreign-alloc
	   dtype
	   :count size
	   :initial-element (coerce-to-dtype 0 dtype))))
    (when *pinned-matrices*
      (push result *pinned-matrices*))
    result))

(defun allocate-mat-with-facet (size obj dtype direction)
  (declare (type fixnum size)
	   (type keyword dtype direction))
  ;; obj= list or simple-array
  (case direction
    (:list
     (let ((result
	     (foreign-alloc
	      dtype
	      :count size
	      :initial-contents (loop for i fixnum upfrom 0 below size
				      collect (nth i obj)))))
       (when *pinned-matrices*
	 (push result *pinned-matrices*))
       result))
    (:simple-array
     (let ((result
	     (foreign-alloc
	      dtype
	      :count size
	      :initial-contents (loop for i fixnum upfrom 0 below size
				      collect (aref obj i)))))
       (when *pinned-matrices*
	 (push result *pinned-matrices*))
       result))
    (:foreign-ptr
     ;; todo: type check
     obj)
    (:foreign-waffe
     (let ((mgl-cube:*let-input-through-p* t))
       (mgl-mat:with-facet (x* ((cl-waffe:data obj)
				'mgl-mat:foreign-array))
	 (slot-value x* 'mgl-mat::base-pointer))))
    (t
     (error "Unknown direction: ~a" direction))))

(defun free-mat (matrix)
  "Frees matrix"
  (declare (type matrix matrix))
  ;; Todo: Free ViewInstruction-ptr
  ;; Todo: check if matrix exists
  ;; Todo: Count total-mem-usage not to forget memfree.
  (unless (matrix-freep matrix) ;; FIXME: Heap Corruption???
    ;(foreign-free (matrix-vec matrix))
    )
  (setf (matrix-freep matrix) t)
  nil)


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

(declaim (ftype (function (list fixnum) fixnum) get-stride))
(defun get-stride (shape dim)
  (declare (optimize (speed 3) (safety 0))
	   (type list shape)
	   (type fixnum dim))
  (let ((subscripts (fill-with-d shape dim)))
    (apply #'+ (maplist #'(lambda (x y)
			    (the fixnum
				 (* (the fixnum (car x))
				    (the fixnum (apply #'* (cdr y))))))
			subscripts
			shape))))

;; Optim it.
(declaim (ftype (function (list) list) calc-strides))
(defun calc-strides (shapes)
  (declare (optimize (speed 3))
	   (type list shapes))
  (loop for i fixnum upfrom 0 below (length shapes)
	collect (get-stride shapes i)))

;;(disassemble #'visible-shape) Optimized
(declaim (ftype (function (list t) list) visible-shape))
(defun visible-shape (orig-shape view)
  "Computes a visible area of orig-shape considering view."
  (declare (optimize (speed 3) (safety 0))
	   (type list orig-shape view))
  (map
   'list
   #'(lambda (view orig-shape)
       (the fixnum
	    (- (view-endindex view orig-shape)
	       (view-startindex view 0))))
   view orig-shape))


;; TODO OPTIMIZE IT
(defun compute-visible-and-broadcasted-shape (shape broadcasts &key (for-print nil))
  (declare (optimize (speed 3)))
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
				(the fixnum (* (the fixnum b) (the fixnum s)))))
			s))
      shape))

(defun print-matrix (matrix stream depth)
  (declare (ignore depth))
  ;; Considering Broadcasts: (1 10) (10 nil) -> (10 10)
  (format stream "<Matrix :~(~a~) :shape ~a :view ~a :visible-shape ~a ~a ~% :vec ~a>"
	  (matrix-dtype matrix)
	  (matrix-shape matrix)
	  (matrix-view matrix)
	  (compute-visible-and-broadcasted-shape
	   (matrix-visible-shape matrix)
	   (matrix-broadcasts matrix)
	   :for-print t)
	  (if (= (matrix-offset matrix) 0)
	      ""
	      (format nil ":offset ~a" (matrix-offset matrix)))
	  (render-matrix matrix :indent 6))) ; TODO: more infos

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
			  (projected-p nil)
			  (strides (calc-strides shape))
			  (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts))))
	    (:constructor
		from-foreign-pointer
		(pointer
		 shape
		 &key
		   (dtype :float)
		 &aux (view (loop repeat (length (the list shape))
				  collect t))
		   (matrix-vec pointer)
		   (broadcasts nil)
		   (projected-p nil)
		   (strides (calc-strides shape))
		   (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts))))
	    (:constructor
		reshape (matrix shape
			 &aux
			   (dtype (dtype matrix))
			   (strides (calc-strides shape))
			   (matrix-vec (matrix-vec matrix))
			   (projected-p nil)
			   (broadcasts nil)
			   (view (loop repeat (length (the list shape))
				       collect t))
			   (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts))))
	    (:constructor
		view-of-matrix (matrix
				broadcasts
				&rest view
				&aux
				  (shape      (matrix-shape matrix))
				  (dtype      (matrix-dtype matrix))
				  (strides    (matrix-strides matrix))
				  (matrix-vec (matrix-vec matrix))
				  (projected-p t)
				  (broadcasts
				   (if broadcasts
				       broadcasts
				       (matrix-broadcasts matrix)))
				  (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts))))
	    (:constructor
		view-of-matrix-with-shape
		(matrix
		 broadcasts
		 visible-shape
		 &rest view
		 &aux
		   (shape      (matrix-shape matrix))
		   (dtype      (matrix-dtype matrix))
		   (strides    (matrix-strides matrix))
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
				   (strides (matrix-strides matrix))
				   (dtype (vec-dtype-quantized quantize-into))
				   (shape (matrix-shape matrix))
				   (view (loop repeat (length (the list shape))
					       collect t))
				   (broadcasts nil)
				   (projected-p
				    (matrix-projected-p matrix))
				   (visible-shape (compute-visible-and-broadcasted-shape (visible-shape shape view) broadcasts)))))
  (freep nil :type boolean)
  (projected-p projected-p :type boolean) ;; Is view-object?
  (vec matrix-vec) ;; The ORIGINAL Matrix's CFFI Pointer
  (dtype dtype :type matrix-dtype)
  (shape shape :type cons) ;; The ORIGINAL Matrix's shape
  (view view :type cons) ;; view instruction
  (external-operation nil)
  (external-operation-dim nil)
  (visible-shape visible-shape :type cons) ;; visible area's shape following viewinstruction
  (broadcasts broadcasts :type list)
  (strides strides :type cons)
  (view-foreign-ptr nil) ;; Todo: Free by free-mat.
  (view-lisp-ptr nil)
  (created-offsets nil :type list)
  (offset 0 :type fixnum))


;; Accessors
(declaim (ftype (function (matrix) index) dims))
(declaim (ftype (function (matrix) list) shape))
(declaim (inline dims shape))

@export
(defun dims (matrix)
  "Returns the length of matrix's dimensions."
  (declare (type matrix matrix)
	   (optimize (speed 3)))
  (length (the list (matrix-visible-shape matrix))))

@export
(defun shape (matrix)
  (declare (optimize (speed 3))
	   (type matrix matrix))
  (matrix-visible-shape matrix))

@export
(defun dtype (matrix)
  (matrix-dtype matrix))


(declaim (inline initialize-views inject-offsets))
(defun initialize-views (view-ptr matrix direction
			 &aux (number-of-dims (dims matrix)))
  ""
  (declare (optimize (speed 3) (safety 0))
	   (type t view-ptr))
  (let* ((m-axis (- number-of-dims 2))
	 (n-axis (- number-of-dims 1))
	 (stride2 (nth m-axis (matrix-strides matrix)))
	 (stride1 (nth n-axis (matrix-strides matrix)))
	 (m       (nth m-axis (shape matrix)))
	 (n       (nth n-axis (shape matrix)))
	 (bc2     (nth m-axis (matrix-broadcasts matrix)))
	 (bc1     (nth n-axis (matrix-broadcasts matrix)))
	 (offset2 (view-startindex (nth m-axis (matrix-view matrix)) 0))
	 (offset1 (view-startindex (nth n-axis (matrix-view matrix)) 0)))
    ;; Match up Broadcasted dims. (e.g.: visible=(10, 10) -> Real: (1, 10))
    (if bc2 (setq m bc2))
    (if bc1 (setq n bc1))

    (if (eql direction :lisp)
	(setf (viewinstruction-lisp-stride2 view-ptr)
	      stride2
	      (viewinstruction-lisp-stride1 view-ptr)
	      stride1
	      (viewinstruction-lisp-m view-ptr)
	      m
	      (viewinstruction-lisp-n view-ptr)
	      n
	      (viewinstruction-lisp-offset2 view-ptr)
	      offset2
	      (viewinstruction-lisp-offset1 view-ptr)
	      offset1
	      (viewinstruction-lisp-broadcast-2 view-ptr)
	      (if bc2
		  0
		  1)
	      (viewinstruction-lisp-broadcast-1 view-ptr)
	      (if bc1
		  0
		  1))
	(setf (foreign-slot-value view-ptr `(:struct ViewInstruction) 'stride2)
	      stride2
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'stride1)
	      stride1
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'm)
	      m
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'n)
	      n
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'offset2)
	      offset2
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'offset1)
	      offset1
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'broadcast2)
	      (if bc2
		  0
		  1)
	      (foreign-slot-value view-ptr `(:struct ViewInstruction) 'broadcast1)
	      (if bc1
		  0
		  1))))
  nil)

(defun inject-offsets (view-ptr direction offset actual-offset)
  ""
  (declare (optimize (speed 3) (safety 0)))
  (if (eql direction :lisp)
      (setf (viewinstruction-lisp-offset view-ptr)
	    offset
	    (viewinstruction-lisp-actual-offset view-ptr)
	    actual-offset)
      (setf (foreign-slot-value view-ptr `(:struct ViewInstruction) 'offset)
	    offset
	    (foreign-slot-value view-ptr `(:struct ViewInstruction) 'actualoffset)
	    actual-offset)))

(defun dtype-as-lisp (matrix)
  (dtype->lisp-type (dtype matrix)))
  
(defmacro dtypecase (matrix &body body)
  `(case (matrix-dtype ,matrix)
     ,@body
     (t
      (error "The operation doesn't support ~a" (matrix-dtype ,matrix)))))

(defmacro assert-dtype (matrix1 matrix2)
  `(assert (eql (matrix-dtype ,matrix1) (matrix-dtype ,matrix2))
	   nil
	   "The dtype of matrices doesn't match. ~a and ~a"
	   (matrix-dtype ,matrix1)
	   (matrix-dtype ,matrix2)))

@export
(defmacro with-pointer-barricade (&body body)
  "All matrices created in this form, are automatically freed."
  `(let ((*pinned-matrices* `(t)))
     (prog1
	 (progn
	   ,@body)
       ;; Add: Display-Report and display mem-usage
       (mapc
	#'(lambda (m)
	    (if (typep m 'matrix)
		(free-mat m)))
	*pinned-matrices*))))

@export
(defmacro with-facet ((var matrix &key (direction 'simple-array))
		      &body body)
  "TMP"
  (case direction
    (:simple-array
     `(let ((,var (convert-into-lisp-array ,matrix :freep nil)))
	(declare (type (simple-array t (*)) ,var))
	,@body))
    (T
     (error "with-facet: Unknown direction: ~a" direction))))

@export
(defmacro with-facets ((&rest forms) &body body)
  (labels ((expand-facet (binding-specs body)
	     (if (endp binding-specs)
		 `(progn ,@body)
		 `(with-facet ,(first binding-specs)
		    ,(expand-facet (cdr binding-specs) body)))))
    (expand-facet forms body)))

(defparameter *unsafe-mode* nil "Ignores type-check")
@export
(defmacro with-unsafe (&body body)
  `(let ((*unsafe-mode* t))
     ,@body))
