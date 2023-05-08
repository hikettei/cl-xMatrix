
(in-package :cl-xmatrix)
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
(defun lisp-type->dtype (lisp-type)
  (cond
    ((eql lisp-type 'single-float) :float)
    ((eql lisp-type 'fixnum) :int)
    ((eql lisp-type '(unsigned-byte 8)) :uint8)  ;;'(integer -256 256))
    ((eql lisp-type '(unsigned-byte 16)) :uint16) ;;'(integer -65536 65536))
    (T (error "The given type is unknown:~a.~% Available dtype is following: ~a" lisp-type *available-dtypes*))))

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

(defun cl-array->foreign-ptr (array size dtype)
  ""
  (declare (type simple-array array)
	   (type keyword dtype)
	   (type fixnum size)
	   (ignorable dtype size))
  #+sbcl
  (sb-sys:vector-sap (sb-ext:array-storage-vector array))
  #-(or sbcl)
  (allocate-mat size :dtype dtype)) ;; Error

;; visible-vec

;; ViewInstruction
;; Matrix-Vec

;; with-facetで, matrix-vecの値などを切り変える。

(defmacro mref ()
  "Matrix-vecarefの補助 strideをもとにIndexを再計算してくれる (Unroll sitai)"
  )

;; Matrix(simple-array, lisp-view-object)
;; |-> CFFIFacet(simple-array.pointer, cffi-view-object)
;; |-> LispFacet(simple-array, lisp-view-object)
;; |-> MetalFacet(simple-array.pointer, cffi-view-object)
;; ...
;; Each vector can be obtained by (matrix-vec matrix)
;; Each facet can be switched with (activate-facet! matrix ~) or with-facet macro.
;;

(defstruct (Matrix  (:print-function print-matrix))
  (active-facet-name 'ForeignFacet :type symbol)
  (freep
   #+sbcl t
   #-sbcl nil
   :type boolean)
  (projected-p nil :type boolean)
  
  (original-vec  nil :type (or null simple-array))
  (original-view nil :type (or null ViewInstruction-Lisp))
  
  (facets nil :type hash-table) ;; Matrix as CFFI Pointer, Simple-Array...
  
  (active-facet nil)

  ;; Shaping APIs
  (dtype :float :type matrix-dtype)
  (shape nil :type cons)
  
  (view nil :type list)
  (visible-shape nil :type list)

  ;; :indices
  (external-operation     nil)
  (external-operation-dim nil)

  ;; :broadcast
  (broadcasts nil  :type list)
  (strides    nil  :type cons)

  ;; Offset APIs (used in the iters located in the deepest)
  (created-offsets nil :type list)
  (offset            0 :type fixnum))

@export
(defun matrix (shape &key (dtype :float) (initial-contents nil) (initial-element nil) (default-facet 'ForeignFacet))
  (let* ((shape (if (= (length shape) 1)
		    `(1 ,@shape)
		    shape))
	 (view (loop for m in shape collect t))
	 (strides (calc-strides shape))
	 (strides-rev (reverse strides))
	 (shape-rev   (reverse shape))
	 (storage (cond
		    ((and initial-element
			  (null initial-contents))
		     (make-array (apply #'* shape)
				 :element-type
				 (dtype->lisp-type dtype)
				 :initial-element
				 initial-element))
		    ((and initial-contents
			  (null initial-element))
		     (make-array (apply #'* shape)
				 :element-type
				 (dtype->lisp-type dtype)
				 :initial-element
				 initial-element))
		    ((and (null initial-element)
			  (null initial-contents))
		     (make-array (apply #'* shape)
				 :element-type
				 (dtype->lisp-type dtype)))
		    (T
		     (error "Couldn't make a new matrix because can't specify both :initial-element and :initial-contents"))))
	 (view-lisp-ptr
	   (view-instruction
	    0
	    0
	    (second shape-rev)
	    (car shape-rev)
	    (second strides-rev)
	    (car strides-rev)
	    0
	    0
	    1
	    1)))
    (let ((result
	    (make-matrix :active-facet-name default-facet
			 :original-vec storage
			 :original-view view-lisp-ptr
			 :facets (make-hash-table)
			 :dtype dtype
			 :shape shape
			 :view view
			 :visible-shape shape
			 :strides strides)))
      (activate-facet! default-facet result)
      result)))
;; FacetのViewの更新どうするか？
;; View -> view-lisp-ptrを更新
;; view-lisp-ptr -> 各FacetのViewにTranscript
;; Side Effects
(defun matrix-update-view! (matrix broadcast-options &rest parsed-view)
  (with-slots ((projected-p projected-p)
	       (visible-shape visible-shape)
	       (broadcasts broadcasts)
	       (view view)
	       (view-struct original-view))
      matrix
    (setf projected-p t
	  view parsed-view
	  broadcasts (if broadcast-options ;; If broadcast-options are specified, use it. Otherwise, keep using the old value.
			 broadcast-options
			 broadcasts))
    ;; Recompute visible-shape
    (setf visible-shape
	  (compute-visible-and-broadcasted-shape
	   (visible-shape (matrix-shape matrix) parsed-view)
	   broadcasts))

    (setf view-struct (view-instruction 0 0 0 0 0 0 0 0 0 0))
    (initialize-views view-struct matrix :lisp)
    matrix))

(defun view-of-matrix (matrix broadcast-options &rest parsed-view)
  ;; Creates a new instance 
  (apply #'matrix-update-view! (copy-matrix matrix) broadcast-options parsed-view))

;; Viewの同期がおかしい気がする。
;; FacetのViewの同期する。
(defun view-of-matrix-with-shape (base-matrix broadcast-options visible-shape &rest parsed-view)
  (let ((view (copy-matrix base-matrix)))
    (with-slots ((projected-p projected-p)
		 (vs          visible-shape)
		 (broadcasts  broadcasts)
		 (viewp view))
	view
      (setf projected-p t
	    vs visible-shape
	    broadcasts (if broadcast-options
			   broadcast-options
			   broadcasts)
	    viewp parsed-view)
      
      (setf (matrix-original-view view) (view-instruction 0 0 0 0 0 0 0 0 0 0))
      (initialize-views (matrix-original-view view) view :lisp)
      view)))

(defun from-foreign-pointer ())
(defun reshape ())

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
