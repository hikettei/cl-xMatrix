
(in-package :cl-xmatrix)

;; This is the my reimplementation of Maddness
;; https://arxiv.org/pdf/2106.10860.pdf
;; Ref: (Bolt, the author's implementation) https://github.com/dblalock/bolt/blob/e7726a4c165cc45ac117e9eabd8761013a26640e/experiments/python/clusterize.py#L1362
;;      (Halutmatmul) https://github.com/joennlae/halutmatmul

;; Wrappers for Mithral's C++ API
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


;; 4.2 Learning the Hash-Function Parameters
;; Bucket B(t, i) where t is the tree's depth and is in the index in the node.

;; Figure:
;;               B(1, 1) <- Possess the original X
;;           /---------------\
;;       B(2, 1)           B(2,2) <- They possess the view of X. (TODO)
;;     /---------\        /--------\
;;   B(3, 1)  B(3, 2)  B(3, 3)  B(3, 4)    
;;
;; In each level of t, 
;;
;; In conclusion, through this operation: I obtain: the optimized thresholds v and split indices j
;; Note: I don't have to make copies of X every time the node goes deeper but alloc space for sumx1, sumx2.
;; Note: B(1, 1) is the special case that the vector is the equivalent to the original. (that is why often I've compared (< d 1))
;; Todo: perhaps I can give the more faster implementation of traning.

(defstruct (MSBucket ;; Bucket
	    (:conc-name bucket-)
	    (:constructor
		make-bucket (&key
			       (d nil)         ;; d=tree's level (i.e.: t)
			       (n 0)           ;; the number of nodes the bucket has.
			       (sumx nil)      ;; For compute losses
			       (sumx2 nil)     ;; For compute losses
			       (point-ids nil) ;;
			       (bucket-id 0)   ;; The bucket's index.
			       (adjustable nil);; allowed to modify? (maybe unused)
			       (dtype :float)  ;; dtype of matrix
			     &aux
			       (point-ids (when (null point-ids)
					    (unless (= N 0)
					      (error "Assetion failed with N = 0"))
					    nil))
			       (n (length point-ids))
			       (id bucket-id)
			       (point-ids point-ids)
			       (d (cond
				    ((and (or (null d) ;; is bucket a top?
					      (< d 1)) 
					  (not (null sumx)))
				     ;; => Bucket posses the original X.
				     (apply #'* (matrix-shape sumx)))
				    ((and (or (null d) ;; is bucket a top?
					      (< d 1))
					  (not (null sumx2)))
				     ;; => Bucket possess the original X.
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
  (point-ids point-ids :type list) ;; the list of indices that could be branch's destination.
  (n n :type index)
  (id id :type fixnum) ;; split-index j1 ... j4
  (adjustable adjustable :type boolean)
  (d d :type fixnum)
  (sumx sumx :type matrix)
  (sumx2 sumx2 :type matrix))


(defmethod destroy-bucket ((bucket MSBucket))
  "Memfree the bucket's non-gc-able matrices"
  (with-slots ((sumx sumx) (sumx2 sumx2)) bucket
    (free-mat sumx)
    (free-mat sumx2)))

(defmethod add-point ((bucket MSBucket) point &key (point-idx nil))
  "B(3, 1)  B(3, 2)  B(3, 3)  B(3, 4) ... <- add B(t, point-idx)
   It isn't evaluated until bucket-split is called."
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
  "Creates the clone of the bucket"
  ;; To fix: don't create copy! the toplevel matrix's view is enough.
  (make-bucket
   :sumx  (bucket-sumx bucket)
   :sumx2 (bucket-sumx2 bucket)
   :point-ids (copy-list (bucket-point-ids bucket))
   :bucket-id bucket-id))

(defmethod bucket-split ((bucket MSBucket)
			 &key
			   (x nil)
			   (dim nil)
			   (val nil)
			   (x-orig nil))
  ;; 書き直す！
  "Given val (threshold v?), splits the bucket, and increment: node-level t+=1.
B(t, ?) -> B(t+1, ?), B(t+1, ??), ...

Algorithm 2 Adding The Next Level to The Hashing Tree.

Bucket-Split: B(A) -> B(id0), B(id1)"
  ;; それぞれの分割した行列は、オリジナルのView
  (let* ((id0 (* 2 (bucket-id bucket)))
 	 (id1 (+ id0 1)))
    (declare (type index id0 id1))

    (when (or (null x)
	      (< (bucket-n bucket) 2))
      ;; Copy of this Bucket + An Empty Bucket.
      (let ((return-value (list (deepcopy bucket :bucket-id id0)
				(make-bucket :d (bucket-d bucket)
					     :bucket-id (bucket-id bucket)))))
	(return-from bucket-split return-value)))

    (if (null (bucket-point-ids bucket))
	(error "Assertion Failed because point-ids=nil"))

    
    ;; Bucket(t, 0) -> Bucket(t+1, ?), Bucket(t+1, ??) ...
    ;; ? = transition-states
    
    (let ((transition-states (bucket-point-ids bucket)))
      (declare (type list transition-states))
      ;;%cmp %lognot

      ;; Create masks for point-idx rows
      ;; And Compares the row[dim] with val.
      ;; dim and val is a parameter to be optimized.

      ;; Note: X and point-idxs are compatible?
      (let* ((x (view x `(:indices ,@transition-states dim) t))
	     (x-mask (%cmp> x val))
	     (not-mask (%lognot x-mask)))

	))))

;; todo: view強化
(defmethod optimal-split-val ((bucket MSBucket) x dim)
  "Returns the split vals j"

  (cond
    ((or (< (bucket-n bucket) 2)
	 (null (bucket-point-ids bucket)))
     (values 0 0))
    (T
     
     )))


;; Computes losses
(defmethod col-variances ((bucket MSBucket))
  (if (< (bucket-n bucket) 1)
      (matrix `(1 ,(bucket-d bucket)) :dtype :float)
      (let ((ex2 (%scalar-mul
		  (%copy (bucket-sumx2 bucket))
		  (/ (bucket-n bucket))))
	    (ex (%scalar-mul
		  (%copy (bucket-sumx bucket))
		  (/ (bucket-n bucket)))))
	(%scalar-add
	 ex2
	 (%scalar-mul
	  (%square ex)
	  -1.0)))))

(defmethod col-sum-sqs ((bucket MSBucket))
  (%scalar-mul (col-variances bucket)
	       (bucket-n bucket)))
    

(defun create-codebook-idxs (D C &key (start-or-end :start))
  "Returning the disjoint indices in this format: [0, 8], [8, 16] ... [start_id, end_id], maddness split the offline matrix A's each row into C. (Prototypes)"
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
				   (nsplits 4) ;; Levels of resuting binary hash tree. (4 is the best).
				   (need-prototypes nil))
  "Training the given prototype, X and X-orig
  X, X-orig = [C, D]"

  ;; Assert:: (> nsplits 4)
  
  ;; NOTE: DONT FORGET MEMFREE
  ;; Note: Operations Bucket does is barely: sum/elwise-mul/elwise-div?
  (let* ((X-copy (%copy X)) ;; X-copy is shared by every buckets.
	 (X-square (%square (%copy X)))
	 (X-orig (%copy x-orig)) ;; X-copy is shared too?
	 (buckets (list
		   ;; Creates the toplevel of bucket.
		   ;; point-ids = 0~N because it has the original X.
		   ;; Semantics: B(1, 0) -> B(2, 0), B(2, 1), ..., B(2, N)
		   (make-bucket
		    :sumx (%sum x :axis 0)
		    :sumx2 (%sum x-square :axis 0)
		    :point-ids (loop for i fixnum upfrom 0 below N
				     collect i))))
	 (splits)
	 
	 ;; The storerooms of losses (should be list?)
	 (col-losses (matrix `(1 ,D) :dtype :float))
	 
	 (offset 0.0)
	 (scal-by 1.0)
	 ;; X' = alpha*X + offset
	 (x-copy (%scalar-add (%scalar-mul X-copy scal-by) offset)))

    ;; これより下でAlloc禁止
    
    (loop repeat nsplits
	  do (progn
	       (%fill col-losses 0.0)
	       
	       (dolist (buck buckets)
		 (let ((loss (col-sum-sqs buck)))
		   (%adds col-losses loss)
		   (free-mat loss)))

	       

	       
	       

	       ))

    (free-mat X-copy)
    (free-mat X-square)
    (free-mat X-orig)
    (free-mat col-losses)

    ))


(defun init-and-learn-mithral (X
			       C
			       ncodebooks
			       &key
				 (K 16))
  "Creates All Prototypes from C, and learning hash-function g(a).
In the maddness paper, the single-float matrix X is compressed into lower bit's matrix, 8bit and restored as it was. however this implementation exceptes to X uint16_t.

X = [N D] (To be optimized)
y = [D M] (To be multiplied)"
  (declare (type matrix x)) ;; X.dtype = :uint16_t

  (assert (= 2 (length (the list (matrix-shape X))))
	  (x)
	  "Assertion Failed with X.dims == 2 ~a"
	  (matrix-shape x))

  (let* ((x-error (%copy X)) ;; Valid Dataset (DONT FORGET: memfree x-error)
	 (x-orig x) ;; Training Dataset (minimize ||a-a'||)
	 (N (car (matrix-shape X)))
	 (D (second (matrix-shape X)))
	 (all-prototypes (matrix `(,C ,K ,D) :dtype (matrix-dtype X)))
	 (all-splits nil) ;; The Result
	 (all-buckets nil) ;; Tmp List
	 ;; indices of disjoints based on C are needed when training KMeans. (j)
	 (pq-idxs (create-codebook-idxs D C :start-or-end :start)))

    (dotimes (cth C) ;; Applying to each prototypes.
      (let ((cth-idx (nth cth pq-idxs)))
	(with-views ((use-x-error x-error t `(,(first cth-idx)
					      ,(second cth-idx)))
		     (use-x-orig  x-orig  t `(,(first cth-idx)
					      ,(second cth-idx))))
	  ;; Iteraiton: [100, D] -> [0~4, D], [4~8, D] ...
	  (multiple-value-bind (msplits protos buckets)
	      (learn-binary-tree-splits
	       use-x-error use-x-orig N D :need-prototypes nil)
	    (declare (ignore protos))

	    ;; Appending prototypes and so on
	    ))))

    (free-mat x-error)

    ))

;; N x D @ D x M
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



