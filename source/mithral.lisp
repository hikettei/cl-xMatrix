
(in-package :cl-xmatrix)

;; Wrappers for Mithral
		  
(defcfun ("mithral_encode_int16_t" mithral-encode) :void
  (X-pointer (:pointer :uint16))
  (nrows :int64)
  (ncols :int)
  (splitdims-pointer (:pointer :uint32))
  (all-splitvals-pointer (:pointer :uint8))
  (shifts (:pointer :uint8))
  (offsets (:pointer :int16))
  (ncodebooks :int)
  (out-pointer (:pointer :uint8)))


(defun randn (matrix)
  (call-with-visible-area matrix #'(lambda (x)
				     (with-view-object (index x)
				       ; this is tmp
				       (setf
					(mem-aref
					 (matrix-vec matrix) :float index)
					(- (random 1.0) 2.5))))))
(defun mithral-square (matrix)
  ;; tmp
  (declare (optimize (safety 0)))
  (call-with-visible-area
   matrix #'(lambda (x)
	      (with-view-object (index x)
		(setf
		 (mem-aref
		  (matrix-vec matrix) :uint16 index)
		 (expt (mem-aref (matrix-vec matrix) :uint16 index) 2))))))

(defun mithral-sum (matrix)
  (let ((total 0))
    (declare (type fixnum total)
	     (optimize (safety 0)))
    (call-with-visible-area
     matrix #'(lambda (x)
		(with-view-object (index x)
		  (incf total
		   (mem-aref
		    (matrix-vec matrix) :uint16 index)))))
    total))

(defun learn_mithral_initialization (X N D ncodebooks)

  )

(defun learn_mithral (X ncodebooks)
  (declare (type matrix x)) ;; X.dtype = :uint16_t

  (assert (= 2 (length (the list (matrix-shape X))))
	  (x)
	  "Assertion Failed with X.dims == 2 ~a"
	  (matrix-shape x))

  (let ((N (car (matrix-shape X)))
	(D (second (matrix-shape X))))
    
    ))

;; NxD @ DxM Todo: Transpose source in advance?
(defclass MithralAMM ()
  ((n :initarg :n :type fixnum :reader mithral-n)
   (d :initarg :d :type fixnum :reader mithral-d)
   (m :initarg :k :type fixnum :reader mithral-m)
   (ncodebooks :initarg :ncodebooks :type fixnum :reader mithral-ncodebooks)
   (lut-work-const :initarg :lut-work-const :type single-float) ;; should be -1?
   ))

(defmethod initialize-instance :after ((mithral mithralAMM) &key &allow-other-keys)
  ;; centroids = (D)
  ;; all-splitvals = (N C)
  ;; out 

  
  
  
  
  
  )

(defmethod encode ((mithral mithralamm) x)
  ;; To ADD: TYPE ASSERT ON LISP
  (mithral-encode
   (matrix-vec x)
   (mithral-n mithral)
   (mithral-d mithral))
  )

(defmethod lut ()
  )

(defmethod scan ()
  )

(defmethod mithral-mm ((mithral mithralamm) x y)

  )

#|
(defun mithral-encode (X-pointer nrows ncols splitdims-pointer all-splitvals-pointer ncodebooks out-pointer dtype)
  (case dtype
    (:int8
     (foreign-funcall "mithral_encode_int8_t"
		      (:pointer :uint8) X-pointer
		      :int nrows
		      :int ncols
		      (:pointer :int32) splitdims-pointer
		      (:pointer :uint8) all-splitvals-pointer
		      :int ncodebooks
		      (:pointer :uint8) out-pointer
		      :void))
    (:float

     )))
|#

(defun test-mithral ()
  ;; X N D where N, D = 32k and D is enough large.
  (let ((x (quantize-matrix (matrix `(64 128))))
	(nrows 64)
	(ncols 128)
	(ncodebooks 4)
	;; splitdims <- 16 * ncodebooks
	(splitdims (matrix `(64) :dtype :int))
	(all-splitvals (quantize-matrix (matrix `(64 128))))
	(out (quantize-matrix (matrix `(64 128)))))
    (mithral-encode
                   (matrix-vec x)
		    nrows
		    ncols
		    (matrix-vec splitdims)
		    (matrix-vec all-splitvals)
		    shifts
		    offsets
		    ncodebooks
		    (matrix-vec out))
    t))

(defun offline-learning ())
(defmacro with-mithral-learning ())



