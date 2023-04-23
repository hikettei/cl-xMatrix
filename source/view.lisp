
(in-package :cl-xmatrix)


;; Conditions Related To Indexing
;;
;;          Indexing-Error (Simple-error)
;;         /                |    
;; View-Indexing-Error Shaping-Error
;;


(define-condition Indexing-Error (simple-error)
  ((content :initarg :content))
  (:documentation "")
  (:report
   (lambda (c s)
     (format s "[cl-xmatrix] Indexing-Error: ~a" (slot-value c 'content)))))

(defmacro indexing-error (content &rest args)
  `(error (make-condition 'indexing-error
			  :content (format nil ,content ,@args))))


(define-condition View-Indexing-Error (indexing-error)
  ((content :initarg :content))
  (:documentation "")
  (:report
   (lambda (c s)
     (format s "[cl-xmatrix] View-Indexing-Error: ~a" (slot-value c 'content)))))

(defmacro view-indexing-error (content &rest args)
  `(error (make-condition 'view-indexing-error
			  :content (format nil ,content ,@args))))

(define-condition Shaping-Error (indexing-error)
  ((content :initarg :content))
  (:documentation "")
  (:report
   (lambda (c s)
     (format s "[cl-xmatrix] Shaping-Error: ~a" (slot-value c 'content)))))

(defmacro shaping-error (content &rest args)
  `(error (make-condition 'Shaping-error
			  :content (format nil ,content ,@args))))

(defmacro assure-dimensions (mat1 mat2)
  "Do nothing if mat1 and mat2 are the same shape, otherwise throw shaping-error"
  `(if (equal (the list (shape ,mat1)) (the list (shape ,mat2)))
       t
       (shaping-error "Two matrices: ~a and ~a couldn't operated together." (shape ,mat1) (shape ,mat2))))

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
		view-instruction (offset actual-offset shape-2 shape-1 stride-2 stride-1 offset-2 offset-1 broadcast-2 broadcast-1)))
  "A Common Instruction for xMatrix, which realise view-function.
ViewInstruction is basically created only for 2d-matrix operation, functions must handle 3d-matrix with with-view macro, and as for 2d is this object.

(m n)"
  (offset offset :type index) ; the nd-dimensional matrix begins with...
  (actual-offset offset :type index)
  (stride2 stride-2 :type index)
  (stride1 stride-1 :type index)
  (offset2 offset-2 :type index) ; the start point
  (offset1 offset-1 :type index) ; the start point (2 3)
  (m shape-2 :type index)
  (n shape-1 :type index)
  (broadcast-2 broadcast-2 :type index)
  (broadcast-1 broadcast-1 :type index))

(declaim (inline view-instruction))

(defcstruct (ViewInstruction :class c-viewinstruction)
  (offset :int)
  (actualoffset :int)
  (stride2 :int)
  (stride1 :int)
  (offset2 :int)
  (offset1 :int)
  (m :int)
  (n :int)
  (broadcast2 :int)
  (broadcast1 :int))

(defmethod translate-from-foreign (ptr (type viewinstruction-lisp))
  (declare (optimize (speed 3)))
  (with-foreign-slots ((offset actualoffset stride2 stride1 offset2 offset1 m n broadcast2 broadcast1) ptr (:struct ViewInstruction))
    (view-instruction offset actualoffset m n stride2 stride1 offset2 offset1 broadcast2 broadcast1)))

(defmethod expand-from-foreign (ptr (type c-ViewInstruction))
  `(with-foreign-slots ((offset actualoffset stride2 stride1 offset2 offset1 m n broadcast2 broadcast1) ,ptr (:struct ViewInstruction))
     (view-instruction offset actualoffset m n stride2 stride1 offset2 offset1 broadcast2 broadcast1)))

(defmethod translate-into-foreign-memory (value (type c-viewinstruction) ptr)
  (transcript-view value ptr))

(defun transcript-view (value ptr)
  (declare (optimize (speed 3)))
  (with-foreign-slots ((offset actualoffset stride2 stride1 offset2 offset1 m n broadcast2 broadcast1) ptr (:struct ViewInstruction))
    (setf offset
	  (viewinstruction-lisp-offset value)
	  actualoffset
	  (viewinstruction-lisp-actual-offset value)
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
	  (viewinstruction-lisp-n value)
	  broadcast2
	  (viewinstruction-lisp-broadcast-2 value)
	  broadcast1
	  (viewinstruction-lisp-broadcast-1 value))
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

(defmacro %* (&rest args)
  `(the fixnum (* ,@(map 'list #'(lambda (x)
				  `(the fixnum ,x))
			 args))))

(defmacro %+ (&rest args)
  `(the fixnum (+ ,@(map 'list #'(lambda (x)
				  `(the fixnum ,x))
			args))))

(defmacro with-view-object ((index view &key (absolute (gensym)))
			    &body body &aux
					 (mi (gensym))
					 (ni (gensym)))
  ;; Todo: Optimize And Inline
  "Given view, iterates body with index.
  :absolute <- index based on the matrix but offset isn't considered."
  `(dotimes (,mi (viewinstruction-lisp-m ,view))
     (dotimes (,ni (viewinstruction-lisp-n ,view))
       (declare (type index ,mi ,ni))
       (let* ((,index (%+ (viewinstruction-lisp-offset ,view)
			  (%*
			   (%*
			    (viewinstruction-lisp-stride2 ,view)
			    (viewinstruction-lisp-broadcast-2 ,view))
			   (+ ,mi (viewinstruction-lisp-offset2 ,view)))
			  (%*
			   (%*
			    (viewinstruction-lisp-stride1 ,view)
			    (viewinstruction-lisp-broadcast-1 ,view))
			   (%+ ,ni (viewinstruction-lisp-offset1 ,view))))) ;; (2 3 4 ...)
	      (,absolute (%+ (viewinstruction-lisp-actual-offset ,view)
			     (%* (viewinstruction-lisp-stride2 ,view)
				 ,mi)
			     (%* (viewinstruction-lisp-stride1 ,view)
				 ,ni)))) ;; (0 1 2 ...)
	 (declare (ignorable ,absolute))
	 ,@body))))

(defmacro with-two-of-views (((index1 view1) (index2 view2) &key (absolute (gensym)))
			     &body
			       body
			     &aux
			       (mi (gensym))
			       (ni (gensym)))
  ""
  `(progn
     (unless (= (viewinstruction-lisp-m ,view1)
		(viewinstruction-lisp-m ,view2))
       (view-indexing-error "two shapes doesn't match:. M1=~a and M2=~a"
			    (viewinstruction-lisp-m ,view1)
			    (viewinstruction-lisp-m ,view2)))
     
     (unless (= (viewinstruction-lisp-n ,view1)
		(viewinstruction-lisp-n ,view2))
       (view-indexing-error "two shapes doesn't match: N1=~a N2=~a"
			    (viewinstruction-lisp-n ,view1)
			    (viewinstruction-lisp-n ,view2)))

     (dotimes (,mi (viewinstruction-lisp-m ,view1))
       (dotimes (,ni (viewinstruction-lisp-n ,view1))
	 (declare (type fixnum ,mi ,ni))
         (let* ((,index1 (%+ (viewinstruction-lisp-offset ,view1)
			     (%*
			      (%* (viewinstruction-lisp-stride2 ,view1)
				  (viewinstruction-lisp-broadcast-2 ,view1))
			      (%+ ,mi (viewinstruction-lisp-offset2 ,view1)))
			     (%* (%*
				  (viewinstruction-lisp-stride1 ,view1)
				  (viewinstruction-lisp-broadcast-1 ,view1))
				 (+ ,ni (viewinstruction-lisp-offset1 ,view1)))))
		(,index2 (%+ (viewinstruction-lisp-offset ,view2)
			     (%*
			      (%*
			       (viewinstruction-lisp-stride2 ,view2)
			       (viewinstruction-lisp-broadcast-2 ,view2))
			      (+ ,mi (viewinstruction-lisp-offset2 ,view2)))
			     (%*
			      (%*
			       (viewinstruction-lisp-stride1 ,view2)
			       (viewinstruction-lisp-broadcast-1 ,view2))
			      (+ ,ni (viewinstruction-lisp-offset1 ,view2)))))
		(,absolute (%+ (viewinstruction-lisp-actual-offset ,view1)
			     (%* (viewinstruction-lisp-stride2 ,view1)
				 ,mi)
			     (%* (viewinstruction-lisp-stride1 ,view1)
				 ,ni))))
	 (declare (type fixnum ,index1 ,index2 ,absolute)
		  (ignorable ,absolute))
	   ,@body)))))

;; Support Repeat -> More Conditions -> Optimize

(defun subscript-p (subscripts matrix &aux (shape (shape matrix)))
  "Returns t if the format of subscripts are correct.
Legal Subscript -> fixnum/list/t, (external-option ~)"
  (declare (optimize (speed 3)))
  (let ((reports))
    (loop for i fixnum upfrom 0
	  for dim in shape
	  for sub in subscripts
	  do (typecase sub
	       (fixnum
		(if (< (the fixnum sub) (the fixnum dim))
		    t
		    (push
		     (format nil "[Axis=~a] Failed with (< subscript[~a] shape[~a]). subscript[~a]=~a shape[~a]=~a~%"
			     i
			     i
			     i
			     i
			     sub
			     i
			     dim)
		     reports)))
	       (list
		(typecase (car sub)
		  (fixnum
		   (cond
		     ;; (5 3) is invaild (>= 5 3)
		     ((>= (the fixnum (car sub)) (the fixnum (second sub)))
		      (push
		       (format nil "[Axis=~a] Failed with (< subscript[~a][0] subscript[~a][1]). subscript=~a~%"
			       i
			       i
			       i
			       sub)
		       reports))
		     ;; (n 10) but axis=3
		     ((> (the fixnum (second sub)) (the fixnum dim))
		      (push
		       (format nil "[Axis=~a] Failed with (< subscript[~a][1] shape[~a]) subscript=~a, shape[~a]=~a~%"
			       i
			       i
			       i
			       sub
			       i
			       dim)
		       reports))
		     ((not (= (length sub) 2))
		      (push
		       (format nil "[Axis=~a] Failed with (= (length subscript[~a]) 2). subscript=~a.~%"
			       i
			       i
			       sub)
		       reports))
		     (t t)))
		  (keyword
		   (case (car sub)
		     (:indices
		      (let ((ov (find-if #'(lambda (x) (>= (the fixnum x) dim)) (the list (cdr sub)))))
			(if ov
			    (push
			     (format nil "[Axis=~a] Each index mustn't exceed ~a, but found: ~a.~%"
				     i
				     dim
				     ov)
			     reports)
			    t)))
		     (:broadcast
		      (cond
			((not (= (length sub) 2))
			 (push
			  (format nil "[Axis=~a] :broadcast keyword is given the following format: `(:broadcast n) but got ~a~%"
				  i
				  sub)
			  reports))
			((not (= (the fixnum dim) 1))
			 (push
			  (format nil "[Axis=~a] The axis to be broadcasted, must be 1 but got ~a.~%" i dim)
			  reports))))
		     (:tflist
		      ;; add type checks
		      t)
		     (T
		      (push
		       (format nil "[Axis=~a] Unknown keyword: ~a~%" i sub)
		       reports))))))
	       (t
		(if (eql sub t)
		    t
		    (push
		     (format nil "[Axis=~a] Invaild argument ~a~%" i sub)
		     reports)))))
    (if reports
	(view-indexing-error
	 "~a"
	 (with-output-to-string (str)
	   (write-string "Couldn't parse subscripts correctly:" str)
	   (write-char #\Newline str)
	   (write-string (format nil "Shape=~a Subscripts=~a~%" shape subscripts) str)
	   (dolist (r (reverse reports))
	     (write-string r str))))
	t)))

(defun compute-absolute-subscripts (orig-mat subscripts)
  "Translate view-subscription into the format which is compatiable with orig-mat"
  (if (matrix-projected-p orig-mat)
      ;; View-Object -> (view) -> View-Object
      (progn
	;; orig-mat turned out to be view-obj, subscripts are relative.
	(let ((old-view (matrix-view orig-mat))
	      (original-shape (matrix-shape orig-mat)))
	  (declare (ignore original-shape)) ;; to be used for error handling
	  ;; This implementation is so ugly ><
	  ;; because the below code describes all cases: (e.g.: Slice->Fixnum, Indices->Fixnum)
	  (labels ((handle-ext-index (view sub)
		     ;; note: don't return sub directly, add view.
		     (typecase view
		       (index
			;; M[2][0]
			(+ view sub))
		       (list
			(typecase (car view)
			  (keyword
			   ;; M[:broadcast 10][1]
			   ;; M[:indices 1 2 3 4][1]
			   (case (car view)
			     (:indices
			      (nth sub (cdr view)))
			     (:broadcast 0)))
			  (index
			   ;; M[2:4].view(1)
			   (+ (car view) sub))
			  (T
			   (view-indexing-error "Can't handle with this instruction: ~a" view))))
		       (t
			;; M[T][0]
			sub)))
		   (handle-ext-range (view sub)
		     (typecase view
		       (index
			;; M[1].view([2:4])
			(view-indexing-error "Attempted to comptute Matrix[~a][~a] but this is out of range." view sub))
		       (list
			(typecase (car view)
			  (keyword
			   ;; M[:broadcast 10][0:2]
			   ;; M[:indices 1 2 3 4][0:2]
			   ;; Todo: Detect out of range
			   (case (car view)
			     (:broadcast
			      `(:indices
				,@(loop for i fixnum upfrom (car sub) below (second sub)
					collect 0)))
			     (:indices
			      `(:indices
				,@(loop for i fixnum upfrom (car sub) below (second sub)
					collect (nth i (cdr view)))))))
			  (index
			   ;; M[2:10][1:2] -> M[3:5]
			   ;; Todo: Detect out of range.
			   `(,(+ (car view) (car sub))
			     ,(+ (car view) (second sub))))
			  (T
			   (view-indexing-error "Cant handle this subscript: ~a" view))))
		       (t sub)))
		   (handle-ext-kw (view sub)
		     (typecase view
		       (index
			;; M[0][:indices 1 2 3]
			(view-indexing-error "Attempted to compute M[~a][~a] but it is out of range." view sub))
		       (list
			(typecase (car view)
			  (keyword
			   ;; M[:broadcast 10][:indices 0 1]
			   ;; M[:indices 1 2 3][:indices 0 1]
			   (case (car view)
			     (:broadcast
			      `(:indices ,@(loop for i upfrom 0 below (second sub) collect 0)))
			     (:indices
			      `(:indices ,@(map
					    'list
					    #'(lambda (k)
						(nth k (cdr view)))
					    (cdr sub))))))
			  (index
			   ;; M[2:6][:indices 1 2]
			   (let ((ls (loop for i fixnum
					   upfrom (car view)
					     below (second view)
					   collect i)))
			     `(:indices ,@(map
					   'list
					   #'(lambda (k)
					       (nth k ls))
					   (cdr sub)))))
			  (T
			   (view-indexing-error "Cant handle this subscript: ~a" view))))
		       ;; M[T][:indices 1 2 3]
		       (t sub)))
		   (handle-ext-kw-broadcast (view sub)
		     (typecase view
		       (index
			;; M[0][:broadcast 10]
			sub)
		       (list
			(typecase (car view)
			  (keyword
			   ;; M[:indices 0 1 2 3][:broadcast 10]
			   ;; M[:broadcast 10][:broadcast 10]
			   (if (and (= (length (car view)) 1)
				    (eql (car view) :indices))
			       ;; Only after [:indices m]
			       sub
			       (view-indexing-error "view: ~a and ~a couldn't broadcasted together. The axis to be broadcasted, must be 1. Also, M[:broadcast 1][:broadcast 1] is prohibited. (Make a copy of matrix and try it again plz.)" view sub)))
			  (index
			   ;; M[0:10][:broadcast 10]
			   (if (= (- (second view) (car view)) 1)
			       sub
			       (view-indexing-error "view: ~a and ~a couldn't broadcasted together. The axis to be broadcasted, must be 1." view sub)))
			  (T
			   (view-indexing-error "Cant handle this subscript: ~a" view))))
		       ;; M[T][:indices 1 2 3]
		       (t sub))))
	    (map 'list #'(lambda (old-view-axis sub)
			   ;; sub = view(orig-axis, old-view)
			   ;; solve on the around way
			   (typecase sub
			     (index
			      (handle-ext-index old-view-axis sub))
			     (list
			      (typecase (car sub)
				(keyword
				 (case (car sub)
				   (:broadcast (handle-ext-kw-broadcast old-view-axis sub))
				   (:indices (handle-ext-kw old-view-axis sub))))
				(index
				 (handle-ext-range old-view-axis sub))
				(t (view-indexing-error "Cant handle this subscript: ~a" sub))))
			     ;; M[:indices 1 2 3][t]
			     (t old-view-axis)))
		 old-view subscripts))))
      ;; Original Matrix -> (view) -> View-Object
      subscripts))

(defun parse-and-replace-tflist-subscripts (matrix
					    subscripts
					    &aux
					      (tf-p
					       #'(lambda (x)
						   (and (typep x 'list)
							(eql (car x)
							     :tflist))))
					      (candiates
					       (position-if
						tf-p
						subscripts)))
  "(:tflist t f t f ...) -> (:indices 1 3 4 ...)
   (:tflist matrix)      -> (:indices 1 3 4 ...)"
  (declare (optimize (speed 3))
	   (type list subscripts))

  (if candiates
      (let* ((tflist (find-if tf-p subscripts))
	     (tflist-args (cdr tflist)))
	(unless (= (count-if tf-p subscripts) 1)
	  (view-indexing-error ":tflist can be used at once in view function's subscript."))
	
	(unless (or (typep tflist-args 'list) ;; check list's elements
		    (typep tflist-args 'matrix))
	  (view-indexing-error ":tflist requires the following: list (consisted of boolean), matrix but got ~a" tflist-args))
	(typecase (car tflist-args)
	  (boolean
	   ;; Todo: Detect (:tflist)
	   (let ((indices (loop for i fixnum upfrom 0
				for tf in tflist-args
				if tf
				  collect i)))
	     (setf (nth candiates subscripts) `(:indices ,@indices))
	     subscripts))
	  (matrix
	   ;; Todo: %satisfies return a matrix of bit.
	   ;; Todo: Detect :tflist mat1 mat2
	   (let* ((mat (car tflist-args))
		  (indices (loop for i fixnum upfrom 0
				   below (nth candiates (shape mat))
				 if (= 1 (round
					  ;; Fixme 1d-mat-aref's return type is unknown.
					  (1d-mat-aref mat i)))
				   collect i)))
	     (setf (nth candiates subscripts) `(:indices ,@indices))
	     subscripts))
	  (T
	   (view-indexing-error ":tflist got invaild argument: ~a. :tflist can be described by list or matrix" (type-of (second tflist-args))))))
      subscripts))

(defun parse-broadcast-subscripts (matrix subscripts)
  "(:broadcast 10) or `(:broadcast t)"
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type list subscripts))
	   
  (let ((broadcasts)
	(newsubs))
    (loop for i fixnum upfrom 0
	  for sub in subscripts
	  do (if (and (typep sub 'cons)
		      (eql (car sub) :broadcast))
		 (progn
		   (unless (= (length sub) 2)
		     (view-indexing-error "Invaild Operation ~a. :broadcast is given in this format:~% `(:broadcast num) num = t or positive fixnum" sub))
		   (unless (= (the index (nth i (matrix-visible-shape matrix))) 1)
		     (view-indexing-error "Can't Broadcast the matrix~%because the axis to be broadcasted is not 1: ~a at axis=~a" (matrix-visible-shape matrix) i))
		   (push (second sub) broadcasts)
		   (push sub newsubs))
		 (progn
		   (push nil broadcasts)
		   (push sub newsubs))))
    (values
     (reverse broadcasts)
     (reverse newsubs))))

(defun straighten-up-subscripts (matrix
				 subscripts
				 &aux (result (loop for i fixnum upfrom 0 below (dims matrix)
						    collect t))
				   (shapes (matrix-visible-shape matrix)))
  "Straighten up subscripts and parse relative-position of matrix.
Example:
  Matrix=(10 10) view=(1)     -> view = (1 t)
  Matrix=(10 10) view=(1 1)   -> view = (1 1)
  Matrix=(10 10) view=(1 1 1) -> Indexing-Error

-1 -> 10 for example."
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type list subscripts))
  (unless (>= (dims matrix) (length subscripts))
    (view-indexing-error "The length of subscripts is too large for the given matrix.~%Matrix:     ~a~%Subscripts: ~a" (matrix-visible-shape matrix) subscripts))

  (labels ((parse-relative-position (dim sub)
	     (typecase sub
	       (fixnum
		(if (>= sub 0)
		    sub
		    (let ((pos (the fixnum (+ (the fixnum (nth dim shapes)) sub))))
		      (if (>= pos 0)
			  pos
			  (view-indexing-error "The relative-index ~a beyonds the corresponding axis ~a.~% Matrix: ~a Subscripts: ~a at axis=~a" sub (nth dim shapes) shapes subscripts dim)))))
	       (list
		(map 'list #'(lambda (x)
			       (parse-relative-position dim x))
		     sub))
	       (T sub))))
    (loop for i fixnum upfrom 0
	  for s in subscripts
	  do (setf (nth i result) (parse-relative-position i s)))
    result))


;; Fixme: x = A[10, 3][:indices 1 2 3, t] + B[1, 3][:indices 1 2 3, t]
(defun view (matrix &rest subscripts
	     &aux
	       (subscripts (straighten-up-subscripts matrix subscripts)))
  "Creates a view-object
subscript is following:

Primitive Operations:
  - fixnum
  - (start-index end-index)
  - t

External Operations: (Speed is not my concern, and it works like macro).
  - (:indices 0 2 4 3 ...) (the length of args is any)
  - (:tflist t nil nil t ...) (the length of args must be correspond with the axis)

>>Detecting errors is todo, good luck :D<<

1. Unwrapped view-object (consisted of primitive operations):
  call-with-visible-area -> CFFI/Common Lisp's function

2. Wrapped view-object by external operations (possess :indices)
  call-with-visible-area ->
       ExternalOperationParser (<- do (dotimes (i indices)))
         -> call-with-visible-area -> CFFI/Common Lisp's function
       Finally, modifies the matrix.

;; Note: view-object -> view-object
;; Semantics:
;; Matrix -> (view) -> View-object is ok.
;; View-object -> (view) -> View-object is undefined. (should be matrix -> view-object)
;; So, view-object -> (view) -> view-object is treated as:
;; view-object -> matrix -> (view-with-nested) -> view-object.

Possibly throws: view-indexing-error

Done: Straighten-up subscripts
      Relative-Indexing
     :tflist"
  
  (declare (optimize (speed 3))
	   (type matrix matrix))

  (subscript-p subscripts matrix)

  (multiple-value-bind (broadcasts subscripts) (parse-broadcast-subscripts matrix subscripts)
    (declare (type list broadcasts subscripts))
    (let* ((subscripts (parse-and-replace-tflist-subscripts matrix subscripts))
	   (subscripts (compute-absolute-subscripts matrix subscripts)))
      (declare (type list subscripts))

      (labels ((external-operations-p (sub)
		 (when (and (typep sub 'list)
			    (typep (car sub) 'keyword))
		   (if (eql (car sub) :indices)
		       t
		       nil))))

	(unless (= (length (matrix-visible-shape matrix))
		   (length subscripts))
	  (error "Assertion Failed with (length matrix.dim) == subscripts (perhaps internal bug)"))
	
	(let ((external-operations (find-if #'external-operations-p subscripts)))
	  (if external-operations
	      (let ((external-operation-dim
		      (position-if #'external-operations-p subscripts)))
		(unless (= (count-if #'external-operations-p subscripts) 1)
		  (view-indexing-error "keyword :indices only appears at once in subscriptions."))
		
		(let ((view-to-return (apply #'view-of-matrix matrix broadcasts subscripts)))
		  (setf (matrix-external-operation view-to-return)
			external-operations)
		  (setf (matrix-external-operation-dim view-to-return)
			external-operation-dim)
		  view-to-return))
	      ;; Otherwise creates view-object normally.
	      (apply #'view-of-matrix matrix broadcasts subscripts)))))))

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
     (typecase (car view)
       (index (the index (car view)))
       (keyword
	(case (car view)
	  (:indices 0)
	  (:broadcast 0)
	  (T
	   (error "view-startindex: unknown keyword"))))
       (T (error "view-startindex: invaild view-instruction fell through"))))
    (index
     (the index view))
    (t
     (the index 0))))

(defun view-endindex (view shape)
  (declare (optimize (safety 0)))
  (typecase view
    (list
     (typecase (car view)
       (index (the index (second view)))
       (keyword
	(case (car view)
	  (:indices (1- (length view)))
	  (:broadcast 1)
	  (T
	   (error "view-endindex: unknown keyword"))))
       (T (error "view-endindex: unknown view-instruction fell through"))))
    (index
     (the index (1+ view)))
    (t
     (the index shape))))

(defun call-with-visible-area-and-extope (matrix function &key (mat-operated-with nil))
  "Handles the external operation of matrix"
  (declare (optimize (speed 3))
	   (type matrix matrix)
	   (type function function))
  (with-slots ((external-operation external-operation)
	       (external-operation-dim external-operation-dim))
      matrix
    (case (car external-operation)
      (:indices
       (let ((indices (cdr external-operation))
	     ;; copy-list: avoid side effects.
	     (view   (copy-list (matrix-view matrix)))
	     ;; Note: Should broadcast's dim considered?
	     (stride (nth external-operation-dim (calc-strides (shape matrix)))))
	 (loop for index in indices
	       for ith fixnum upfrom 0
	       do (let ((view (copy-list view)))
		    (setf (nth external-operation-dim view) index)
		    (let ((matrix* (apply #'view-of-matrix matrix nil view)))
		      (call-with-visible-area
		       matrix*
		       function
		       :mat-operated-with mat-operated-with
		       :first-offset (the fixnum
					  (* (the fixnum stride) (the fixnum ith))))))))
       nil)
      (T
       (error "Can't handle with unknown ext-operation ~a" external-operation))))
  nil)


(defun call-with-visible-area (matrix function
			       &key
				 (mat-operated-with nil)
				 (first-offset 0))
  "Under this macro, three or more dimensions matrix are expanded into 2d, and set index-variable ViewInstruction.

function - #'(lambda (lisp-structure) body)

matrix shouldn't possess broadcasted axis while mat-operated-with is ok.

Returns - nil"
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function)
	   (type fixnum first-offset))

  ;; Assert matrix doesn't have broadcast

  ;; check if matrix's subscript include :indices
  (when (let ((op (matrix-external-operation matrix)))
	  (and
	   op
	   (not (eql (car op) :broadcast))))
    (let ((mat (if (and mat-operated-with
			(matrix-external-operation mat-operated-with))
		   (progn
		     (format t "Warning: call-with-visible-area copied mat-operated-with~%")
		     (%copy mat-operated-with))
		   mat-operated-with)))
      (return-from call-with-visible-area
	(call-with-visible-area-and-extope
	 matrix
	 function
	 :mat-operated-with mat))))
  
  (let ((dims (matrix-shape matrix))
	(views (matrix-view matrix))
	(strides (matrix-strides matrix))
	(broadcasts (matrix-broadcasts matrix))
	(broadcasts1 (if mat-operated-with
			 (matrix-broadcasts mat-operated-with)
			 nil)))
    (labels ((explore-batch (total-offset  ;; Offsets considered broadcast
			     actual-offset ;; No broadcast (for output)
			     dim-indicator
			     rest-dims)
	       (declare (type index total-offset actual-offset dim-indicator rest-dims))
	       (cond
		 ((> rest-dims 2)
		  (let* ((view-point (nth dim-indicator views))
			 (start-with (view-startindex view-point 0))
			 (end-with   (view-endindex view-point (nth dim-indicator dims)))
			 (incn 1) ; increment
			 (stride (the index (nth dim-indicator strides)))
			 (1+dims-indicator (1+ dim-indicator))
			 (1-rest-dims (1- rest-dims))
			 (start-with1 (if mat-operated-with
					  (view-startindex (nth dim-indicator (matrix-view mat-operated-with)) 0)
					  nil))
			 (repeat (if broadcasts
				     (nth dim-indicator broadcasts)
				     nil))
			 (repeat-act (if broadcasts1
					 (nth dim-indicator broadcasts1)
					 nil))
			 (stride1 (if repeat
				      0
				      stride))
			 (stride (if repeat-act
				     0
				     stride)))
		    (declare (type index start-with end-with stride 1+dims-indicator 1-rest-dims))
		    
		    (if (or repeat-act repeat)
			(if (typep repeat 'index)
			    (setq end-with (+ start-with repeat))
			    (setq end-with (cond
					     ((and broadcasts1
						   (eql repeat t)
						   (eql repeat-act t))
					      (1+ start-with))
					     ((and broadcasts1
						   (eql repeat t))
					      (the index (+ start-with (the index repeat-act))))
					     (t
					      (1+ start-with))))))

		    (let ((offsets (+ total-offset (the index (* start-with stride1))))
			  (actual-offsets
			    (if mat-operated-with
				(+ actual-offset (the index (* start-with1 stride)))
				actual-offset)))
		      
		      (loop for i fixnum upfrom start-with below end-with by incn
			    do (progn
				 (explore-batch offsets
						actual-offsets
						1+dims-indicator
						1-rest-dims)
				 (incf offsets stride1)
				 ;; ActualOffsets incrementing forcibly.
				 (incf actual-offsets stride))))))
		 ((= rest-dims 2)
		  (let* ((dim (nth dim-indicator dims))
			 (1+dim (nth (1+ dim-indicator) dims))
			 (view-point (nth dim-indicator views))
			 (1+view-point (nth (1+ dim-indicator) views))
			 (offset2 (view-startindex view-point 0))
			 (offset1 (view-startindex 1+view-point 1+dim))
			 (offset2e (view-endindex view-point dim))
			 (offset1e (view-endindex 1+view-point 1+dim))
			 (repeat2 (if broadcasts
				      (nth dim-indicator broadcasts)
				      nil))
			 (repeat1 (if broadcasts
				      (nth (1+ dim-indicator) broadcasts)
				      nil))
			 (stride2 (nth dim-indicator strides))
			 (stride1 (nth (1+ dim-indicator) strides))
			 (shape2 (the index (- offset2e offset2)))
			 (shape1 (the index (- offset1e offset1))))

		    (if repeat2
			(setq shape2 repeat2))
		    (if repeat1
			(setq shape1 repeat1))

		    (let ((instruction (view-instruction
					total-offset
					actual-offset
					shape2
					shape1
					stride2
					stride1
					offset2
					offset1
					(if repeat2
					    0
					    1)
					(if repeat1
					    0
					    1))))
		      ;; (when (= 1+dim 1)
		      ;;       transpose ~~~)
		      ;; (M 1) -> (1 M) (M 1 fails to SIMD)

		      (if mat-operated-with
			  (let* ((t-views (matrix-view mat-operated-with))
				 (t-dims (matrix-shape mat-operated-with))
				 (tstrides (matrix-strides mat-operated-with))
				 (t-view-point (nth dim-indicator t-views))
				 (t-view-point+1 (nth (1+ dim-indicator) t-views))
				 (t-dim (nth dim-indicator t-dims))
				 (t-dim+1 (nth (1+ dim-indicator) t-dims))
				 
				 (t-offset2 (view-startindex t-view-point 0))
				 (t-offset1 (view-startindex t-view-point+1 t-dim+1))
				 (t-offset2e (view-endindex t-view-point t-dim))
				 (t-offset1e (view-endindex t-view-point+1 t-dim+1))
				 (trepeat2 (if broadcasts1
					       (nth dim-indicator broadcasts1)
					       nil))
				 (trepeat1 (if broadcasts1 
					       (nth (1+ dim-indicator) broadcasts1)
					       nil))
				 (tstrides2 (nth dim-indicator tstrides))
				 (tstrides1 (nth (1+ dim-indicator) tstrides))
				 (tshape2 (the index (- t-offset2e t-offset2)))
				 (tshape1 (the index (- t-offset1e t-offset1))))
			    
			    (if trepeat2
				(setq tshape2 trepeat2))
			    (if trepeat1
				(setq tshape1 trepeat1))

			    (let ((instruction1
				    (view-instruction
				     actual-offset
				     actual-offset
				     tshape2
				     tshape1
				     tstrides2
				     tstrides1
				     t-offset2
				     t-offset1
				     (if trepeat2
					 0
					 1)
				     (if trepeat1
					 0
					 1))))
			      (funcall function instruction instruction1)))
			  (funcall function instruction)))))
		 ((= rest-dims 1)
		  ;; Reshaping `(M) into `(1 M) and regard it as 2d MAT
		  (setq dims `(1 ,@dims))
		  (setq views `(t ,@views))
		  (setq strides (calc-strides dims))
		  (explore-batch
		   0
		   0
		   0
		   2))
		 (T (error "Scalar value fell through.")))
	       nil))

      ;; getting ugly ><
      (explore-batch
       0
       first-offset
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
  "Cacheに指定された次元数の行列をAlloc. gemm!など二次元でまとめてIterationして欲しい時に使う（予定）"
  )

(defmacro with-broadcasting (out-shape (index1 matrix1) (index2 matrix2) &body body)
  "
Usage:

(with-broadcasting out-dim (mat1* mat1) (mat2* mat2)
     (let ((result (matrix out-dim)))
        (%adds result mat1*)
        (%adds result mat2*)))

"
  )


(defun convert-into-lisp-array (matrix &key (freep nil))
  "Convert matrix's visible area into common lisp's simple array"
  (let ((returning-array (make-array
			  (apply #'* (shape matrix))
			  :element-type t ;; FixMe
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

(defun from-lisp-array (matrix lisp-array)
  "Matrix <- Lisp-Array"
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x :absolute index1)
				       (setf (mem-aref (matrix-vec matrix)
						       (matrix-dtype matrix)
						       index)
					     (aref lisp-array index1))))))

