
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

;; First _learn_hash_buckets_and_prototype
;; For first, impl some arithmetic ops.

;; Never forget to memfree
(defstruct (MSBucket
	    (:conc-name bucket-)
	    (:constructor
		make-bucket (&key
			       (d nil)
			       (n 0)
			       (sumx nil)
			       (sumx2 nil)
			       (point-ids nil)
			       (bucket-id 0)
			       (adjustable nil)
			       (dtype :float)
			     &aux
			       (point-ids (when (null point-ids)
					    (unless (= N 0)
					      (error "Assetion failed with N = 0"))
					    nil))
			       (n (length point-ids))
			       (id bucket-id)
			       (point-ids point-ids)
			       (d (cond
				    ((and (or (null d)
					      (< d 1))
					  (not (null sumx)))
				     (apply #'* (matrix-shape sumx)))
				    ((and (or (null d)
					      (< d 1))
					  (not (null sumx2)))
				     (apply #'* (matrix-shape sumx2)))
				    (t
				     (if (null d)
					 (error "Assertion Failed because d is nil")
					 d))))
			       (sumx (if sumx
					 sumx
					 (matrix `(,d) :dtype dtype)))
			       (sumx2 (if sumx2
					  sumx2
					  (matrix `(,d) :dtype dtype))))))
  (point-ids point-ids :type list)
  (n n :type index)
  (id id :type fixnum)
  (adjustable adjustable :type boolean)
  (d d :type fixnum)
  (sumx sumx :type matrix)
  (sumx2 sumx2 :type matrix))


(defmethod destroy-bucket ((bucket MSBucket))
  (with-slots ((sumx sumx) (sumx2 sumx2)) bucket
    (free-mat sumx)
    (free-mat sumx2)))

(defmethod add-point ((bucket MSBucket) point &key (point-idx nil))
  (unless (bucket-adjustable bucket)
    (error "The bucket is not adjustable."))

  (incf (bucket-n bucket) 1)
  (%scalar-add (bucket-sumx bucket) point)
  (%scalar-add (bucket-sumx2 bucket) (expt point 2))

  (push point-idx (bucket-point-ids bucket))
  (setf (bucket-point-ids bucket)
	(delete-duplicates (bucket-point-ids bucket) :test #'=))
  nil)

(defmethod remove-point ((bucket MSBucket) point &key (point-idx nil))
  (unless (bucket-adjustable bucket)
    (error "The bucket is not adjustable."))

  (decf (bucket-n bucket) 1)
  (%scalar-add (bucket-sumx bucket) (- point))
  (%scalar-add (bucket-sumx2 bucket) (- (expt point 2)))

  (setf (bucket-point-ids bucket)
	(delete-if #'(lambda (x) (= x point-idx)) (bucket-point-ids bucket)))
  nil)

(defmethod deepcopy ((bucket MSBucket)
		     &key (bucket-id nil)
		     &aux
		       (bucket-id (if bucket-id
				      bucket-id
				      (bucket-id bucket))))
  (make-bucket
   :sumx (%copy (bucket-sumx bucket))
   :sumx2 (%copy (bucket-sumx2 bucket))
   :point-ids (copy-list (bucket-point-ids bucket))
   :bucket-id bucket-id))

(defmethod bucket-split ((bucket MSBucket)
			 &key
			   (x nil)
			   (dim nil)
			   (val nil)
			   (x-orig nil))
  "Note: Returns a list"
  (let* ((id0 (* 2 (bucket-id bucket)))
 	 (id1 (+ id0 1)))

    (when (or (null x)
	      (< (bucket-n bucket) 2))
      (let ((return-value (list (deepcopy bucket :bucket-id id0)
				(make-bucket :d (bucket-d bucket)
					     :bucket-id (bucket-id bucket)))))

	;; should delete current bucket? (destroy-bucket bucket)
	;; if the bucket is never used...
	(return-from bucket-split return-value)))

    (if (null (bucket-point-ids bucket))
	(error "Assertion Failed because point-ids=nil"))

    ;; Bucket(0) -> Bucket(1), Bucket(2), Bucket(3), ...

    (let ((result)
	  (transition-states (bucket-point-ids bucket)))
      (dolist (transition transition-states)
	;; x-orig[N, D], x-orig-t [1~3, D] etc...
	(let* ((x-t (unless (< transition (car (matrix-visible-shape x-orig)))
		      nil
		      (view x-orig transition t)))
	       (x-orig-t x-t)
	       (mask (%cmp x-orig-t val)) ;; val is scalar
	       (not-mask (%lognot mask))
	       
	))))

(defun create-codebook-idxs (D C &key (start-or-end :start))
  "
    returns vector (C, 2)
    [
      start_idx_0, end_idx_0,
      start_idx_1, end_idx_1,
      ...
    ]
  "
  (declare (optimize (speed 3))
	   (type index D C)
	   (type keyword start-or-end))

  (unless (find start-or-end (list :start :end))
    (error "start-or-end = :start :end"))

  (unless (>= D C)
    (error "Assertion Failed with D >= C"))
  ;; Simply, perhaps it should be rewrriten like:
  ;; (loop for i fixnum upfrom 0 below D by (round (/ D C))
  ;; collect (list (* i (round (/ D C))) (* (1+ i) (round (/ D C)))))
  (let ((full-len-subvec (round (/ D C)))
	(start-idx 0))
    (declare (type index start-idx full-len-subvec))
    (let ((result
	    (loop for n fixnum upfrom 0 below C
		  collect (let ((subvec-len full-len-subvec))
			    (declare (type index subvec-len))
			    (case start-or-end
			      (:start
			       (if (< n (mod D C))
				   (incf subvec-len 1)))
			      (:end
			       (if (< (+ C (- n) (- 1)) (mod D C))
				   (incf subvec-len 1))))
			    (let ((end-idx (min D (the index (+ start-idx subvec-len)))))
			      (prog1
				  (list start-idx end-idx)
				(setq start-idx end-idx)))))))
      ;; Todo: Make it matrix?
      result)))

(defun learn-binary-tree-splits (X
				 x-orig
				 N
				 D
				 &key
				   (nsplits 4)
				   (need-prototypes nil))
  ;; dont forget memfree them
  (let* ((X (%copy X))
	 (X-square (%square (%copy x)))
	 (X-orig (%copy x-orig))
	 (buckets (list
		   (make-bucket
		    :sumx (%sum x :axis 0)
		    :sumx2 (%sum x-square :axis 0)
		    :point-ids (loop for i fixnum upfrom 0 below N
				     collect i))))
    )

(defun init-and-learn-mithral (X C ncodebooks)
  "Learns and initializes hash-function, g(a) and prototypes."
  (declare (type matrix x)) ;; X.dtype = :uint16_t

  (assert (= 2 (length (the list (matrix-shape X))))
	  (x)
	  "Assertion Failed with X.dims == 2 ~a"
	  (matrix-shape x))

  (let* ((x-error (%copy X)) ;; memfree x-error
	 (x-orig x)
	 (K 16)
	 (N (car (matrix-shape X)))
	 (D (second (matrix-shape X)))
	 (all-prototypes (matrix `(,C ,K ,D) :dtype (matrix-dtype X)))
	 (all-splits nil)
	 (all-buckets nil)
	 ;; indices of disjoints based on C are needed when training KMeans.
	 (pq-idxs (create-codebook-start-and-end-idxs X C)))

    (dotimes (cth C)
      (let ((cth-idx (nth cth pq-idxs)))
	(with-views ((use-x-error x-error t `(,(first cth-idx)
					      ,(second cth-idx)))
		     (use-x-orig  x-orig  t `(,(first cth-idx)
					      ,(second cth-idx))))
	  (multiple-value-bind (msplits protos buckets)
	      (learn-binary-tree-splits use-x-error use-x-orig N D :need-prototypes nil)
	    (declare (ignore protos))

	    ))))))

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



