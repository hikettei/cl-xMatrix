
(in-package :cl-xmatrix)

(annot:enable-annot-syntax)
;; Facet API Handling multiple facet of array.

;;
;;
;; CFFI Pointer <-> Simple-Array
;;                  Array
;;                  
;;

;; Excepted Workflow:
;;
;; a = matrix(hogehoge) One initializes a new matrix with CFFI Pointer
;; Work with SIMD/C, If complicated operation needed, write extensions in Lisp.
;; No Overheads between a<->SIMD<->Lisp Kernel
;;

;; Using CLOS
;; 目標 AMM/MaddnessのEncoding時間の短縮
;; Export MaddnessMatmul.

;; 扱うデータは全て一次元の行列となる
;; (defmacro subscript (hoge) ~)

;; First Class <- Simple-Array
;; -> Calling CFFI... with foreign-ptr
;; -> Writing Kernel With ... simple-array

;; 設計を考える

;; AbstractMatrix

;; -> define-backend matrix (with generics)
;; Handling in multiple devices, and 微分可能に


(defclass Facet-Of-Matrix ()
  ((original-mat :initarg :orig-mat :reader facet-orig-mat)
   (vec :initarg :vec :reader facet-vec :writer write-facet-vec)
   (view :initarg :view :reader facet-view :writer write-facet-view)))


(defgeneric synchronize-vec  (facet array))
(defgeneric synchronize-view (facet view))

(defun synchronize-facet-and-matrix (facet &aux (orig-mat (facet-orig-mat facet)))
  (when (null (matrix-active-facet orig-mat))
    (activate-facet! (matrix-active-facet-name orig-mat)
		     orig-mat)))

(defun synchronize-facet (matrix)
  (when (null (matrix-active-facet matrix))
    (activate-facet! (matrix-active-facet-name matrix)
		     matrix)))

(defmethod initialize-instance :after ((facet Facet-Of-Matrix) &key &allow-other-keys)
  (with-slots ((vec vec) (view view)) facet
    (synchronize-vec  facet vec)
    (synchronize-view facet view)))

(defclass Simple-Array-Facet (Facet-Of-Matrix)
  ((original-mat :initarg :orig-mat :reader facet-orig-mat)
   (vec  :initarg :vec  :reader facet-vec  :writer write-facet-vec :type simple-array)
   (view :initarg :view :reader facet-view :writer write-facet-view :type viewinstruction-lisp))
  (:documentation "An fundamental facet of Lisp-Array (simple-array)"))

(defmethod synchronize-vec ((facet Simple-Array-Facet) array)
  (write-facet-vec array facet))

(defmethod synchronize-view ((facet Simple-Array-Facet) view)
  (write-facet-view view facet))


#+sbcl(progn
(defclass ForeignFacet (Facet-Of-Matrix)
  ((original-mat :initarg :orig-mat :reader facet-orig-mat)
   (vec :initarg :vec :reader facet-vec :writer write-facet-vec)
   (view :initarg :view :reader facet-view :writer write-facet-view))
  (:documentation "An fundamental facet of CFFI Array. If cl-xMatrix is working on SBCL, CFFI pointer and Lisp-Array are automatically synchronized, otherwise synchronized manually."))

(defmethod synchronize-vec ((facet ForeignFacet) array)
  (write-facet-vec (cl-array->foreign-ptr
		    (facet-vec facet)
		    (array-total-size array)
		    (lisp-type->dtype (array-element-type array)))
		   facet))

(defmethod synchronize-view ((facet ForeignFacet) view)
  (write-facet-view
   (let ((foreign-view (foreign-alloc '(:struct ViewInstruction))))
     (initialize-views foreign-view (facet-orig-mat facet) :foreign)
     foreign-view)
   facet)))

;; #-sbcl <- TODO

(defun make-facet (matrix facet-name storage view)
  (make-instance facet-name :orig-mat matrix :vec storage :view view))


@export
(defun activate-facet! (facet-name matrix &key (if-doesnt-exist :create))
  "TODO: DOC"
  (declare (type (and keyword (member :create :error)) if-doesnt-exist)
	   (type matrix matrix))

  (let ((target-facet (or
		       (gethash facet-name (matrix-facets matrix))
		       (case if-doesnt-exist
			 (:create
			  (setf (gethash facet-name (matrix-facets matrix))
				(make-facet matrix facet-name (matrix-original-vec matrix) (matrix-original-view matrix))))
			 (:error
			  (error "FacetNotFound")
			  ;; Add Conditions: FacetNotFound
			  )))))
    (setf (matrix-active-facet matrix) target-facet)))

@export
(defun matrix-vec (matrix)
  ""
  (declare (type matrix matrix))
  (let ((active-facet (matrix-active-facet matrix)))
    (facet-vec active-facet)))


;; 基本的に、incf-view!などで変更するのは、orig-view
;; matrix-view-ptrが呼び出された時、MOVEする。
@export
(defun matrix-view-ptr (matrix)
  ""
  (declare (type matrix matrix))
  (let ((active-facet (matrix-active-facet matrix)))
    (if (eql (matrix-direction matrix) :lisp)
	(copy-viewinstruction-lisp (matrix-original-view matrix))
	(let ((ptr (facet-view active-facet)))
	  (transcript-view (matrix-original-view matrix) ptr)
	  ptr))))

@export
(defmacro slot-view (matrix name &key (direction :lisp))
  "TODO: DOC"
  `(if (eql ,direction :lisp)
      (slot-value (matrix-view-ptr ,matrix) ',name)
      (foreign-slot-value (matrix-view-ptr ,matrix) '(:struct ViewInstruction) ',name)))

(defmacro incf-slot-view (matrix name incr &key (direction :lisp))
  "TODO: DOC"
  `(if (eql ,direction :lisp)
      (incf (the fixnum (slot-value (matrix-view-ptr ,matrix) ',name)) ,incr)
      (incf (the fixnum (foreign-slot-value (matrix-view-ptr ,matrix) '(:struct ViewInstruction) ',name)) ,incr)))

(defmacro setf-slot-view (matrix name value &key (direction :lisp))
  "TODO: DOC"
  `(if (eql ,direction :lisp)
      (setf (slot-value (matrix-view-ptr ,matrix) ',name) ,value)
      (setf (foreign-slot-value (matrix-view-ptr ,matrix) '(:struct ViewInstruction) ',name) ,value)))

@export
(defun matrix-direction (matrix)
  (typecase (facet-view (matrix-active-facet matrix))
    (ViewInstruction-Lisp :lisp)
    (t :foreign)))


;; Facet APIs


(defmacro with-facet (var (matrix facet-name))

  )
