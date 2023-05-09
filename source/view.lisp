
(in-package :cl-xmatrix)

(annot:enable-annot-syntax)

(deftype subscript-t ()
  "An list of types which is allowed to become subscripts of the function view."
  `(or fixnum list null t))


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
  "Do nothing if mat1 and mat2 are the same shape, otherwise throw shaping-error."
  `(unless *unsafe-mode*
     (if (equal (the list (shape ,mat1)) (the list (shape ,mat2)))
	 t
	 (shaping-error "Two matrices: ~a and ~a couldn't operated together." (shape ,mat1) (shape ,mat2)))))


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


;; Memos:
;; offset, actualoffset -> Determined when call-with-visible-area is called.
;; others -> Determined when view/matrix is created.

;; An alias of ViewInstruction for CFFI.
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
  "CFFI ptr -> Lisp ViewInstruction"
  (declare (optimize (speed 3)))
  (with-foreign-slots ((offset actualoffset stride2 stride1 offset2 offset1 m n broadcast2 broadcast1) ptr (:struct ViewInstruction))
    (view-instruction offset actualoffset m n stride2 stride1 offset2 offset1 broadcast2 broadcast1)))

;; Inlining Version.
(defmethod expand-from-foreign (ptr (type c-ViewInstruction))
  "CFFI prt -> Lisp ViewInstruction"
  `(with-foreign-slots ((offset actualoffset stride2 stride1 offset2 offset1 m n broadcast2 broadcast1) ,ptr (:struct ViewInstruction))
     (view-instruction offset actualoffset m n stride2 stride1 offset2 offset1 broadcast2 broadcast1)))

#+sbcl(declaim (ftype (function (ViewInstruction-Lisp sb-sys:system-area-pointer) sb-sys:system-area-pointer) transcript-view))
(declaim (inline transcript-view))
(defun transcript-view (value ptr)
  "Transcripts value(ViewInstruction-Lisp)'s slots into ptr (ViewInstruction-C)"
  (declare (optimize (speed 3) (safety 0)))
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

(defmethod translate-into-foreign-memory (value (type c-viewinstruction) ptr)
  "Lisp ViewInstruction -> CFFI Pointer"
  (declare (inline transcript-view))
  (transcript-view value ptr))

(defmethod expand-into-foreign-memory (value (type c-viewinstruction) ptr)
  `(transcript-view ,value ,ptr))

(defmacro %* (&rest args)
  "Utils"
  `(the fixnum (* ,@(map 'list #'(lambda (x)
				  `(the fixnum ,x))
			 args))))

(defmacro %+ (&rest args)
  "Utils"
  `(the fixnum (+ ,@(map 'list #'(lambda (x)
				  `(the fixnum ,x))
			args))))

(defmacro with-view-object ((index view &key (absolute (gensym)))
			    &body body &aux
					 (mi (gensym))
					 (ni (gensym)))
  ;; Todo: Optimize And Inline, Docs
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

(defmacro with-view-visible-strides ((index view &key (absolute (gensym)))
				     &body body &aux
						  (mi (gensym))
						  (ni (gensym)))
  ;; Todo: Optimize And Inline, Docs
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
	      (,absolute (%+ (%* (viewinstruction-lisp-n ,view) ,mi) ,ni))) ;; (0 1 2 ...)
	 (declare (ignorable ,absolute))
	 ,@body))))

(defmacro with-two-of-views (((index1 view1) (index2 view2) &key (absolute (gensym)))
			     &body
			       body
			     &aux
			       (mi (gensym))
			       (ni (gensym)))
  "Todo: Docs Computes index given two matrices"
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

(declaim (ftype (function (fixnum subscript-t fixnum) list) find-subscript-error)
	 (inline find-subscript-error))
(defun find-subscript-error (i sub dim &aux (reports nil))
  "Finding view-indexing-error in advance.
Args:
- i     the axis
- sub   subscripts[axis]
- dim   visible-shape[axis]

Return: List (Consisted of strings which records error log)"
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum i dim)
	   (type subscript-t sub))
  (typecase sub
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
	  reports))))
  reports)


(defun compute-absolute-subscript (old-view subscript)
  "Translate view-subscription into the format which is compatiable with orig-mat"
  (declare (optimize (speed 3) (safety 0))
	   (type subscript-t old-view subscript))

  (labels ((handle-ext-index (view sub)
	     ;; note: don't return sub directly, add view.
	     (typecase view
	       (index
		;; M[2][0]
		(the index (+ view (the index sub))))
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
		   (the index
			(+ (the index (car view)) (the index sub))))
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
		   `(,(the index (+ (the index (car view)) (the index (car sub))))
		     ,(the index (+ (the index (car view)) (the index (second sub))))))
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
		      `(:indices ,@(loop for i upfrom 0 below (the index (second sub)) collect 0)))
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
		   (if (and (= (length (the list (cdr view))) 1)
			    (eql (car view) :indices))
		       ;; Only after [:indices m]
		       sub
		       (view-indexing-error "view: ~a and ~a couldn't broadcasted together. The axis to be broadcasted, must be 1. Also, M[:broadcast 1][:broadcast 1] is prohibited. (Make a copy of matrix and try it again plz.)" view sub)))
		  (index
		   ;; M[0:10][:broadcast 10]
		   (if (= (- (the index (second view)) (the index (car view))) 1)
		       sub
		       (view-indexing-error "view: ~a and ~a couldn't broadcasted together. The axis to be broadcasted, must be 1." view sub)))
		  (T
		   (view-indexing-error "Cant handle this subscript: ~a" view))))
	       ;; M[T][:indices 1 2 3]
	       (t sub))))
    
    (typecase subscript
      (index (handle-ext-index old-view subscript))
      (list
       (typecase (car subscript)
	 (keyword
	  (case (car subscript)
	    (:broadcast (handle-ext-kw-broadcast old-view subscript))
	    (:indices   (handle-ext-kw           old-view subscript))))
	 (fixnum
	  (handle-ext-range old-view subscript))
	 (t (view-indexing-error "Cannot handle with this subscript: ~a" subscript))))
      (t old-view))))

(declaim (ftype (function (fixnum subscript-t matrix fixnum) (values subscript-t subscript-t)) parse-broadcast))
(defun parse-broadcast (orig-shape subscript matrix axis)
  "If subscript is broadcast. Returns the broadcasted shape and new-subscript. do nothing otherwise."
  (declare (optimize (speed 3) (safety 0)) 
	   (type fixnum orig-shape axis)
	   (type subscript-t subscript)
	   (type matrix matrix))

  (if (and (typep subscript 'list)
	   (eql (car subscript) :broadcast))
      (progn
	(unless (= (length subscript) 2)
	  (view-indexing-error "Invaild Operation ~a. :broadcast is given in this format:~% `(:broadcast num) num = t or positive fixnum" subscript))
	(unless (= orig-shape 1)
	  (view-indexing-error "Can't Broadcast the matrix~%because the axis to be broadcasted is not 1: ~a at axis=~a" (shape matrix) axis))
	(values t (second subscript)))
      (values subscript nil)))


;; FixME: Optimize this function.
(declaim (ftype (function (fixnum subscript-t) subscript-t) replace-tflist)
	 (inline replace-tflist))
(defun replace-tflist (orig-shape subscript)
  "Replace :tflist into :indices if exists, otherwise do nothing."
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum orig-shape)
	   (type subscript-t subscript))

  (if (and (typep subscript 'list)
	   (eql (car subscript) :tflist))
      (typecase (second subscript)
	(boolean
	 (if (> (length subscript) orig-shape)
	     (view-indexing-error "The size of :tflist beyonds the axis. ~a" orig-shape))
	 (let ((indices (loop for i fixnum upfrom 0
			      for tf in (cdr subscript)
			      if (eql tf t)
				collect i)))
	   `(:indices ,@indices)))
	(matrix
	 ;; Todo: %satisfies return a matrix of bit.
	 ;; Todo: Detect :tflist mat1 mat2
	 ;; 1.0 = True, 0.0 = False.
	 (let* ((mat (second subscript))
		(indices (loop for i fixnum upfrom 0
				 below orig-shape
			       if (locally (declare (optimize (speed 3)))
				    (= 1 (round
					  (1d-mat-aref mat i))))
				 collect i)))
	   `(:indices ,@indices)))
	(t
	 (view-indexing-error "The type ~a cannot be used as :tflist's arguments." (type-of (second subscript)))))
      subscript))

(declaim (ftype (function (subscript-t) (values subscript-t subscript-t))
		parse-external-operation)
	 (inline parse-external-operation))
(defun parse-external-operation (subscript)
  "Parses :indices if exists otherwise do nothing."
  (declare (type subscript-t subscript)
	   (optimize (speed 3) (safety 0)))
  (if (and (typep subscript 'list)
	   (eql (car subscript) :indices))
      (values subscript subscript)
      (values subscript nil)))

(declaim (ftype (function (fixnum subscript-t) subscript-t)))
(defun parse-relative-position (orig-shape sub)
  "Parses relatie-position like: -1, -2...
specifing :- means orig-shape (todo: write docs)"
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum orig-shape)
	   (type subscript-t sub))
  (typecase sub
    (fixnum
     (if (>= sub 0)
	 sub
	 (let ((pos (the fixnum (+ orig-shape sub))))
	   (if (>= pos 0)
	       pos
	       (view-indexing-error "The relative index ~a beyonds the original axis ~a" sub orig-shape)))))
    (list
     (map 'list #'(lambda (x) (parse-relative-position orig-shape x)) sub))
    (T (if (eq sub :~)
	   orig-shape
	   sub))))

(defmacro unroll-maplist ((var iter-num) &body body)
  (labels ((mkstr (&rest args)
	     (with-output-to-string (s)
	       (dolist (a args) (princ a s))))
	   
	   (symb (&rest args)
	     (values (intern (apply #'mkstr args))))
	   
	   (retain-objects (name i)
	     `(list ,@(loop for k fixnum upfrom 0 below i
			    collect (symb name k))))
	   (step-iter (i)
	     (if (>= i 0)
		 `(multiple-value-bind
			(,(symb 'subscript i)
			 ,(symb 'broadcast i)
			 ,(symb 'visible-shape i)
			 ,(symb 'external-operation i)
			 ,(symb 'external-operation-dim i)
			 ,(symb 'error-str i))
		      (let ((,var ,i)) ,@body)
		    ,(step-iter (1- i)))
		 `(values
		   ,(retain-objects 'subscript iter-num)
		   ,(retain-objects 'broadcast iter-num)
		   ,(retain-objects 'visible-shape iter-num)
		   ,(retain-objects 'external-operation iter-num)
		   ,(retain-objects 'external-operation-dim iter-num)
		   ,(retain-objects 'error-str iter-num)))))
    (step-iter (1- iter-num))))

(declaim (inline compute-visible-size)
	 (ftype (function (subscript-t subscript-t) fixnum) compute-visible-size))
(defun compute-visible-size (shape view)
  (declare (optimize (speed 3) (safety 0)))
  (the fixnum
       (- (view-endindex view shape)
	  (view-startindex view 0))))

(declaim (ftype (function (fixnum
			   matrix
			   fixnum
			   (or fixnum list null t)
			   (or fixnum list null t)
			   boolean)
			  (values t t t t t t))
		parse-subscript-by-axis)
	 (inline parse-subscript-by-axis))
(defun parse-subscript-by-axis (axis
				matrix
				orig-shape
				orig-view
				subscript
				padding-subscript)
  "Returning -> (values parsed-subscript[sub] broadcast[Fixnum] visible-shape[fixnum] external-operation[null or list]) external-operation-dim-num[fixnum] Errors[null or string]"
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum orig-shape)
	   (type boolean padding-subscript)
	   (type (or fixnum list t) orig-view subscript))

  ;; Works:
  ;; Detect View error
  ;; compute visible-shape
  ;; compute absolute view
  ;; Separate Broadcasting
  ;; compute ext-ops (:indices) (:tflist)

  ;; Here, Make -1 -> 1 (Compute Absolute)
  ;; If :tflist, convert it into :indices
  
  (let* ((subscript (parse-relative-position orig-shape subscript))
	 (subscript (replace-tflist orig-shape subscript)) ;; :tflist -> :indices
	 (subscript (if padding-subscript ;; Padding
			t
			subscript)))

    (multiple-value-bind (subscript broadcast) (parse-broadcast orig-shape subscript matrix axis) ;; Find out broadcasts
      (multiple-value-bind (subscript external-operation) (parse-external-operation subscript) ;; Find out :indices
	(if (matrix-projected-p matrix)
	    (let ((subscript (compute-absolute-subscript orig-view subscript)))
	      (values
	       subscript
	       broadcast
	       (or broadcast
		   (compute-visible-size orig-shape subscript))
	       external-operation
	       (if external-operation
		   axis)
	       ;; Error check will be done in: Original-Mat <-> View
	       (find-subscript-error axis subscript (nth axis (matrix-shape matrix)))))
	    ;; Simply, matrix -> View
	    (values
	     subscript
	     broadcast
	     (or broadcast
		 (compute-visible-size orig-shape subscript))
	     external-operation
	     (if external-operation
		 axis)
	     (find-subscript-error axis subscript orig-shape)))))))

;; Optimize: The case when subscript contains :indices/:tflist
(defun parse-subscripts (matrix
			 subscripts
			 &aux
			   (orig-shape (shape matrix))
			   (orig-view  (matrix-view  matrix))
			   (dimensions (dims matrix))
			   (subscript-len (length subscripts)))
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type list subscripts orig-shape orig-view)
	   (type fixnum dimensions))
  ;; Assertion: (100% as long as created by matrix) (length orig-shape) == (length orig-view)
  
  (unless (>= dimensions subscript-len)
    (view-indexing-error
     "The length of subscripts is too large for the given matrix.~%Matrix:     ~a~%Subscripts: ~a"
     (matrix-visible-shape matrix) subscripts))

  ;; SBCL's IRC Bug: Unsafe concurrent operations on #<HASH-TABLE :TEST EQL :COUNT 14 {100D1E9F03}> detected, the following ugly case clauses below can't be rewritten with more briefly notations/macros ;_;.
  (case dimensions
    (1
     (unroll-maplist (i 1)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (2
     (unroll-maplist (i 2)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (3
     (unroll-maplist (i 3)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (4
     (unroll-maplist (i 4)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (5
     (unroll-maplist (i 5)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (6
     (unroll-maplist (i 6)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (7
     (unroll-maplist (i 7)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (8
     (unroll-maplist (i 8)
       (parse-subscript-by-axis
	i
	matrix
	(nth i orig-shape)
	(nth i orig-view)
	(nth i subscripts)
	(>= i subscript-len))))
    (T
     (let ((subs)
	   (bcs)
	   (vshapes)
	   (ext-opes)
	   (ext-dims)
	   (error-list))
       (dotimes (i dimensions)
	 (multiple-value-bind
	       (subscript
		broadcast
		visible-shape
		external-operation
		ext-ope-num
		err)
	     (parse-subscript-by-axis i matrix (nth i orig-shape) (nth i orig-view) (nth i subscripts) (>= i subscript-len))
	   (push subscript subs)
	   (push broadcast bcs)
	   (push visible-shape vshapes)
	   (push external-operation ext-opes)
	   (push ext-ope-num ext-dims)
	   (push err error-list)))
       (values
	(reverse subs)
	(reverse bcs)
	(reverse vshapes)
	(reverse ext-opes)
	(reverse ext-dims)
	(reverse error-list))))))

;; 2x times faster compared to old definition
;; Memo: f(x: A∈R^(m n)) -> g(offsets, B∈R^(1 n))
;; Unroll and JIT g
(declaim (ftype (function (matrix &rest subscript-t) matrix) view))
(defun view (matrix &rest subscripts)
  "Todo: Docstring
  Creates a view-object
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
	   (type matrix matrix)
	   (type list subscripts))
  (multiple-value-bind (subscripts
			broadcasts
			visible-shape
			external-operation
			external-dims
			errors)
      (parse-subscripts matrix subscripts)
    (declare (type list subscripts visible-shape external-dims external-operation errors))

    (when (find-if #'(lambda (x) x) errors)
      (view-indexing-error
       "~a"
       (with-output-to-string (out)
	 (format out "~%Creating a new view object was failed because:~%")
	 (dolist (errors-per-axis errors)
	   (dolist (err errors-per-axis)
	     (when err
	       (princ "   " out)
	       (princ err out))))
	 (format out "~%The target matrix's visible area:~% -> ~a~%The subscripts:~% -> ~a~%"
		 (shape matrix)
		 subscripts))))
    (let ((place (find-if #'(lambda (x) x) external-dims)))
      (if place
	  (let ((external-operation (nth place external-operation))
		(view-to-return (apply #'view-of-matrix-with-shape
				       matrix
				       broadcasts
				       visible-shape
				       subscripts)))
	    (unless (= (count-if #'(lambda (x) x) external-dims) 1)
	      (view-indexing-error "The keyword :tflist, :indices can be used at once in one view's subscripts."))
	    (setf (matrix-external-operation view-to-return) external-operation
		  (matrix-external-operation-dim view-to-return) place)
	    view-to-return)
	  (progn
	    (apply #'view-of-matrix-with-shape matrix broadcasts visible-shape subscripts))))))

(defun incf-view! (matrix axis increment &aux (dims (dims matrix)))
  "Modifies the matrix's view-object.
subscript <- must not include indices"
  (declare (optimize (speed 3))
	   (type matrix matrix)
	   (type fixnum axis increment dims))

  ;; if axis in m/n, douzini incr ptr.
  (labels ((add-n (obj)
	     (typecase obj
	       (list (map 'list #'add-n obj))
	       (fixnum (the fixnum (+ obj increment)))
	       (T (error "incf-view! the target axis possess this view object:~a but it couldn't be used with incf-view!. This is due to with-view was invaild, the axis you specified is a view of original matrix?" (nth axis (matrix-view matrix)))))))
    (let ((new-view (add-n (nth axis (matrix-view matrix)))))

      ;; kokokara saikai
      (when (> (the fixnum
		    (typecase new-view
		      (fixnum new-view)
		      (list (second new-view))))
	       (the fixnum (nth axis (matrix-shape matrix))))
	(view-indexing-error "incf-view! increments beyonds matrix's size"))

      (setf (nth axis (matrix-view matrix)) new-view)

      (when (= axis (- dims 2))
	;; axis=M
	(incf (viewinstruction-lisp-offset2 (matrix-original-view matrix)) increment))
      
      (when (= axis (- dims 1))
	;; axis=N
	(incf (viewinstruction-lisp-offset1 (matrix-original-view matrix)) increment))
      nil)))

@export
(defun set-view! (matrix axis new-index)
  "Alias for system-set-view! no guarantee of this function"
  (system-set-view! matrix axis new-index))

(defun system-set-view! (matrix axis new-index &aux (dims (dims matrix)))
  "DO NOT EXPORT IT"
  (declare (optimize (speed 3))
	   (type matrix matrix)
	   (type fixnum axis new-index dims))

  (unless (typep (nth axis (matrix-view matrix)) 'fixnum)
    (error "system-set-view! Internal Error. (the axis to be replaced must be FIXNUM view.)"))

  (setf (nth axis (matrix-view matrix)) new-index)

  (when (= axis (- dims 2))
    ;; axis=M
    (incf (viewinstruction-lisp-offset2 (matrix-original-view matrix)) new-index))

  (when (= axis (- dims 1))
    ;; axis=N
    (incf (viewinstruction-lisp-offset1 (matrix-original-view matrix)) new-index))
  nil)


;; Todo: :indices lambda(x), :tflist lambda(x)
;; Is there a bug?
(defun incf-offsets! (matrix &rest increments)
  "The function incf-view is the faster way to modify matrices' visible area.
Todo: Example"
  (declare (optimize (speed 3))
           (type matrix matrix)
	   (type list increments))

  (unless (matrix-projected-p matrix)
    (warn "incf-offsets! received an un-viewed matrix."))
  
  (let ((total 0)
	(created-offsets (or (matrix-created-offsets matrix)
			     (setf (matrix-created-offsets matrix)
				   (loop for i fixnum upfrom 0 below (dims matrix)
					 collect 0)))))
    (declare (type fixnum total))
    
    (mapc
     #'(lambda (orig-size
		visible-size
		existing-offset
		stride
		incr &aux (updated-size (the fixnum (+ visible-size existing-offset incr))))
	 (declare (type fixnum orig-size visible-size existing-offset stride incr))
	 (let ((increment (* stride incr)))
	   (declare (type fixnum increment))
	   (unless (and
		    (<= updated-size (the fixnum (1+ orig-size)))
		    (>= updated-size 0))
	     (view-indexing-error "incf-offsets! couldn't update the matrix's offsets~%because adding a new increments ~a into the created offsets ~a~%would exceed the range of matrix's shape. ~a" increments created-offsets (shape matrix)))
	   (incf total increment)))
     (matrix-shape matrix)
     (shape matrix)
     created-offsets
     (matrix-strides matrix)
     increments)

    (dotimes (i (dims matrix))
      (let ((incr (nth i increments)))
	(when incr
	  (incf (the fixnum (nth i created-offsets))
		(the fixnum incr)))))

    (incf (matrix-offset matrix) total)
    
    (matrix-offset matrix)))

(defun reset-offsets! (matrix)
  "Todo :docs"
  (declare (optimize (speed 3))
	   (type matrix matrix))
  (setf (matrix-created-offsets matrix) nil
	(matrix-offset matrix) 0)
  nil)

(defmacro with-view ((var matrix &rest subscripts)
		     &body body)
  ""
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


(declaim (ftype (function (t fixnum) fixnum) view-startindex view-endindex)
	 (inline view-startindex view-endindex))
(defun view-startindex (view _)
  (declare (optimize (speed 3) (safety 0))
	   (ignore _))
  (typecase view
    (list
     (typecase (car view)
       (fixnum (the fixnum (car view)))
       (keyword
	(case (car view)
	  (:indices 0)
	  (:broadcast 0)
	  (T
	   (error "view-startindex: unknown keyword"))))
       (T (error "view-startindex: invaild view-instruction fell through"))))
    (fixnum
     (the fixnum view))
    (t
     (the fixnum 0))))

(defun view-endindex (view shape)
  (declare (optimize (speed 3) (safety 0)))
  (typecase view
    (list
     (typecase (car view)
       (fixnum (the fixnum (second view)))
       (keyword
	(case (car view)
	  (:indices (1- (length view)))
	  (:broadcast 1)
	  (T
	   (error "view-endindex: unknown keyword"))))
       (T (error "view-endindex: unknown view-instruction fell through"))))
    (fixnum
     (the fixnum (1+ view)))
    (t
     (the fixnum shape))))

(defun call-with-visible-area-and-extope (matrix function &key (mat-operated-with nil) (direction :lisp))
  "Handles the external operation of matrix"
  (declare (optimize (speed 3))
	   (type matrix matrix)
	   (type function function)
	   (inline call-with-visible-area))
  (with-slots ((external-operation external-operation)
	       (external-operation-dim external-operation-dim))
      matrix
    
    (case (car external-operation)
      (:indices
       (let ((indices (cdr external-operation))
	     ;; copy-list: avoid side effects.
	     (view   (copy-list (matrix-view matrix)))
	     ;; FixME?: Should broadcast's dim considered?
	     (stride (nth external-operation-dim (calc-strides (shape matrix)))))

	 (setf (nth external-operation-dim view) 0)

	 (let ((m* (apply #'view-of-matrix matrix nil view)))
	   (loop for index in indices
		 for ith fixnum upfrom 0
		 do (progn
		      (system-set-view! m* external-operation-dim index)
		      (call-with-visible-area
		       m*
		       function
		       :direction direction
		       :mat-operated-with mat-operated-with
		       :first-offset (the fixnum
					  (* (the fixnum stride) (the fixnum ith))))))))
       nil)
      (T
       (error "Can't handle with unknown ext-operation ~a" external-operation))))
  nil)

;; Had I had but a strongly statically typed language like Coalton!
;; I could inline call-with-visible-area... (It remains to be optimized, since it could unrolled and compield by SBCL.)
;; Note: To reduce call-with-visible-area's overheads ultimately, we would need the information about the shape and viewi information about the array to be computed. Fortunately, view/broadcasting's definition make it ez if only i have coalton.

;; Theorically, I can unroll this iteration but ANSI CL's APIs are insufficient!.
;; f(args) -> f(offsets, args)
(declaim (ftype (function (matrix function &key
				  (:mat-operated-with (or null matrix))
				  (:first-offset fixnum)
				  (:direction keyword))
			  null)
		call-with-visible-area))
(defun call-with-visible-area (matrix function
			       &key
				 (mat-operated-with nil)
				 (first-offset 0)
				 (direction :lisp))
  "Under this macro, three or more dimensions matrix are expanded into 2d, and set index-variable ViewInstruction.

function - #'(lambda (lisp-structure) body)

matrix shouldn't possess broadcasted axis while mat-operated-with is ok.

Returns - nil

Constraints: matrix.dims == mat-operated-with.dims, matrix.dims >= 2."
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix)
	   (type function function)
	   (type fixnum first-offset)
	   (type keyword direction))

  ;; TODO: Assert matrix.dims >= 2 (otherwise reshape and recursive it)

  (unless (or (eql direction :lisp)
	      (eql direction :foreign))
    (error "Unknwon direction ~a. Available directions: :lisp :foreign" direction))

  (if (eql direction :lisp)
      (activate-facet! 'Simple-Array-Facet matrix)
      (activate-facet! 'ForeignFacet matrix))

  (when mat-operated-with
    (if (eql direction :lisp)
	(activate-facet! 'Simple-Array-Facet mat-operated-with)
	(activate-facet! 'ForeignFacet mat-operated-with)))
  
  ;; Assert matrix doesn't have broadcast
  ;; check if matrix's subscript include :indices (not :broadcast)
  
  (when (let ((op (matrix-external-operation matrix)))
	  (and
	   op
	   (not (eql (car op) :broadcast))))
    (let ((mat (if (and mat-operated-with
			(matrix-external-operation mat-operated-with))
		   (progn

		     ;; Under (speed 3) declartion
		     (format t "Warning: call-with-visible-area copied mat-operated-with~%")
		     (%copy mat-operated-with))
		   mat-operated-with)))
      (return-from call-with-visible-area
	(call-with-visible-area-and-extope
	 matrix
	 function
	 :mat-operated-with mat
	 :direction direction))))

  
  (let ((dims (matrix-shape matrix))
	(views (matrix-view matrix))
	(strides (matrix-strides matrix))
	(broadcasts (matrix-broadcasts matrix))
	(broadcasts1 (if mat-operated-with
			 (matrix-broadcasts mat-operated-with)
			 nil))
	(view-ptr1 (matrix-view-ptr matrix))
	(view-ptr2 (when (not (null mat-operated-with))
		     (matrix-view-ptr mat-operated-with))))

    
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
		  ;; Instruction1 -> total-offset actual-offset
		  ;; Instruction2 -> actual-offset, actual-offset

		  (inject-offsets view-ptr1 direction total-offset actual-offset)
		  (unless (null mat-operated-with)
		    (inject-offsets view-ptr2 direction actual-offset actual-offset))

		  (if (null mat-operated-with)
		      (funcall function view-ptr1)
		      (funcall function view-ptr1 view-ptr2)))
		 ((= rest-dims 1)
		  ;; Reshaping `(M) into `(1 M) and regard it as 2d MAT
		  (error "Not Supported (TODO)")

		  )
		 (T (error "Scalar value fell through.")))
	       nil))
      
      (explore-batch
       (matrix-offset matrix)
       (if mat-operated-with
	   (+ (matrix-offset mat-operated-with)
	      first-offset)
	   first-offset)
       0
       (length dims))

      (inject-offsets view-ptr1 direction 0 0)
      
      (when (not (null mat-operated-with))
	(inject-offsets view-ptr2 direction 0 0))
	    
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
    (call-with-visible-area
     matrix
     #'(lambda (x)
	 (with-view-visible-strides (index x :absolute index1)
	   (setf (aref returning-array index1)
		 (aref (matrix-vec matrix) index))))
     :direction :lisp)
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

@export
(defun from-facet (shape obj &key (direction :simple-array) (dtype :float))
  "list/array -> matrix"
  (from-foreign-pointer
   (allocate-mat-with-facet (apply #'* shape) obj dtype direction)
   shape
   :dtype dtype))
