
(in-package :cl-xmatrix)

(defun print-view (view stream depth)
  (declare (ignore depth))
  (format stream "=ViewInstruction========~%  total_offset:    ~a~%  strides:       (~a ~a)~%  offsets:       (~a ~a)~%  visible_shape: (~a ~a)~%"
	  (viewinstruction-lisp-offset view)
	  (viewinstruction-lisp-stride2 view)
	  (viewinstruction-lisp-stride1 view)
	  (viewinstruction-lisp-offset2 view)
	  (viewinstruction-lisp-offset1 view)
	  (viewinstruction-lisp-m view)
	  (viewinstruction-lisp-n view)))

(defstruct (ViewInstruction-Lisp
	    (:print-function print-view)
	    (:constructor
		view-instruction (offset shape-2 shape-1 stride-2 stride-1 offset-2 offset-1)))
  "A Common Instruction for xMatrix, which realise view-function.
ViewInstruction is basically created only for 2d-matrix operation, functions must handle 3d-matrix with with-view macro, and as for 2d is this object.

(m n)"
  (offset offset :type index) ; the nd-dimensional matrix begins with...
  (stride2 stride-2 :type index)
  (stride1 stride-1 :type index)
  (offset2 offset-2 :type index) ; the start point
  (offset1 offset-1 :type index) ; the start point (2 3)
  (m shape-2 :type index)
  (n shape-1 :type index))


(defcstruct (ViewInstruction :class c-viewinstruction)
  (offset :int)
  (stride2 :int)
  (stride1 :int)
  (offset2 :int)
  (offset1 :int)
  (m :int)
  (n :int))

(defmethod translate-from-foreign (ptr (type viewinstruction-lisp))
  (with-foreign-slots ((offset stride2 stride1 offset2 offset1 m n) ptr (:struct ViewInstruction))
    (view-instruction offset m n stride2 stride1 offset2 offset1)))

(defmethod expand-from-foreign (ptr (type c-ViewInstruction))
  `(with-foreign-slots ((offset stride2 stride1 offset2 offset1 m n) ,ptr (:struct ViewInstruction))
     (view-instruction offset m n stride2 stride1 offset2 offset1)))

(defmethod translate-into-foreign-memory (value (type c-viewinstruction) ptr)
  (transcript-view value ptr))

(defun transcript-view (value ptr)
  (with-foreign-slots ((offset stride2 stride1 offset2 offset1 m n) ptr (:struct ViewInstruction))
    (setf offset
	  (viewinstruction-lisp-offset value)
	  stride2
	  (viewinstruction-lisp-stride2 value)
	  stride1
	  (viewinstruction-lisp-stride1 value)
	  offset2
	  (viewinstruction-lisp-offset2 value)
	  offset1
	  (viewinstruction-lisp-offset1 value)
	  m
	  (viewinstruction-lisp-m value)
	  n
	  (viewinstruction-lisp-n value))
    ptr))

(defmacro with-expanding-view-object (view &body body)
  "Note: this macro is depcrecated"
  `(with-slots ((offset offset)
	        (stride2 stride2)
	        (stride1 stride1)
		(offset2 offset2)
		(offset1 offset1)
		(m n)
		(n n))
       (the ViewInstruction-Lisp ,view)
     (declare (ignorable offset stride2 stride1 offset2 offset1 m n))
     ,@body))

(defmacro with-view-object ((index view &key (absolute (gensym)))
			    &body body &aux
					 (mi (gensym))
					 (ni (gensym)))
  "Given view, iterates body with index."
  `(dotimes (,mi (viewinstruction-lisp-m ,view))
     (dotimes (,ni (viewinstruction-lisp-n ,view))
       (let* ((,index (+ (viewinstruction-lisp-offset ,view)
			 (* (viewinstruction-lisp-stride2 ,view)
			    (+ ,mi (viewinstruction-lisp-offset2 ,view)))
			 (* (viewinstruction-lisp-stride1 ,view)
			    (+ ,ni (viewinstruction-lisp-offset1 ,view))))) ;; (2 3 4 ...)
	      (,absolute (+ (viewinstruction-lisp-offset ,view)
			    (* (viewinstruction-lisp-stride2 ,view)
			       ,mi)
			    (* (viewinstruction-lisp-stride1 ,view)
			       ,ni)))) ;; (0 1 2 ...)
	 (declare (ignorable ,absolute))
	 ,@body))))


(defun subscript-p (subscripts)
  "Returns t if the format of subscripts are correct."
  t)

(defun subscript-compatiable (matrix subscripts)
  "Returns t if the subscripts are compatiable to matrix."
  t)

(defun view (matrix &rest subscripts)
  "Creates a view-object"
  (declare (type (satisfies subscript-p) subscripts)
	   (type Matrix matrix))
  ; supply the lack of dims.
  (apply #'view-of-matrix matrix subscripts))

(defmacro with-view ((var matrix &rest subscripts) &body body)
  `(let ((,var (view ,matrix ,@subscripts)))
     ,@body))

(defmacro with-views ((&rest forms) &body body)
  "(with-view a1 (with-view a2 ... ))"
  (labels ((expand-views (binding-specs body)
	     (if (endp binding-specs)
		 `(progn ,@body)
		 `(with-view ,(first binding-specs)
		    ,(expand-views (cdr binding-specs) body)))))
    (expand-views forms body)))
       

(declaim (ftype (function (t fixnum) index) view-startindex view-endindex)
	 (inline view-startindex view-endindex))
(defun view-startindex (view _)
  (declare (ignore _))
  (typecase view
    (list
     (the index (car view)))
    (index
     (the index view))
    (t
     (the index 0))))

(defun view-endindex (view shape)
  (declare (optimize (safety 0)))
  (typecase view
    (list
     (the index (second view)))
    (index
     (the index (1+ view)))
    (t
     (the index shape))))


(defun call-with-visible-area (matrix function)
  "Under this macro, three or more dimensions matrix are expanded into 2d, and set index-variable ViewInstruction.

function - #'(lambda (lisp-structure) body)

Returns - nil"
  (declare (optimize (speed 3))
	   (type matrix matrix)
	   (type function function))
  (let ((dims (matrix-shape matrix))
	(views (matrix-view matrix))
	(strides (matrix-strides matrix)))
    (labels ((explore-batch (total-offset
			     dim-indicator
			     rest-dims)
	       (declare (type index total-offset dim-indicator rest-dims))
	       (cond
		 ((> rest-dims 2)
		  (let* ((view-point (nth dim-indicator views))
			 (start-with (view-startindex view-point 0))
			 (end-with   (view-endindex view-point (nth dim-indicator dims)))
			 (incn 1) ; increment
			 (stride (nth dim-indicator strides))
			 (1+dims-indicator (1+ dim-indicator))
			 (1-rest-dims (1- rest-dims)))
		    (declare (type index start-with end-with stride 1+dims-indicator 1-rest-dims))
		    (let ((offsets total-offset))
		      (loop for i fixnum upfrom start-with below end-with by incn
			    do (progn
				 (explore-batch offsets
						1+dims-indicator
						1-rest-dims)
				 (incf offsets stride))))))
		 ((= rest-dims 2)
		  (let* ((dim (nth dim-indicator dims))
			 (1+dim (nth (1+ dim-indicator) dims))
			 (view-point (nth dim-indicator views))
			 (1+view-point (nth (1+ dim-indicator) views))
			 (offset2 (view-startindex view-point 0))
			 (offset1 (view-startindex 1+view-point 1+dim))

			 (offset2e (view-endindex view-point dim))
			 (offset1e (view-endindex 1+view-point 1+dim)))
		    (let ((instruction (view-instruction
					total-offset
					(the index (- offset2e offset2))
					(the index (- offset1e offset1))
					(nth dim-indicator strides)
					(nth (1+ dim-indicator) strides)
					offset2
					offset1)))

#|
Note:
		      (M 1) fails to be paralellized by SIMD.
		      Reshaping (M 1) into (1 M) may work. (TODO for performance)
|#
		      (with-foreign-object (c '(:struct ViewInstruction))
			(funcall function instruction)
			))))
		 ((= rest-dims 1)
		  ; (M) regard as (M 1)

		  ; fixme
		  (setq dims `(,@dims 1))
		  (setq views `(,@views 1))
		  (setq strides `(,@strides 1))
		  (explore-batch
		   0
		   1
		   2))
		 (T (error "Scalar value fell through.")))
	       nil))

      (explore-batch
       0
       0
       (length dims))
      nil)))

;; (disassemble #'matrix-visible-row-major-index)
(defun matrix-visible-row-major-index (matrix &rest indices)
  "Todo: Error check"
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix))
  (let ((strides (matrix-strides matrix))
	(total 0))
    (declare (type index total))
    (loop for i fixnum upfrom 0 below (length strides)
	  do (incf total
		   (the index (* (the index (nth i strides))
				 (the index (nth i indices))))))
    total))

(defmacro within-2d-view ()

  )

;Todo Broadcasting is with-visible-areaをまとめて二回
(defmacro with-broadcasting ((index1 matrix1) (index2 matrix2) &body body)

  )

; dokka ugokasu

(defun convert-into-lisp-array (matrix &key (freep nil))
  "Convert matrix's visible area into common lisp's simple array"
  (let ((returning-array (make-array
			  (apply #'* (matrix-visible-shape matrix))
			  :element-type t ; fixme
			  )))
    (call-with-visible-area matrix #'(lambda (x)
				       (with-view-object (index x :absolute index1)
					 (setf (aref returning-array index1)
					       (mem-aref (matrix-vec matrix)
							 (matrix-dtype matrix)
							 index)))))
    (if freep
	(free-mat matrix))
    returning-array))
