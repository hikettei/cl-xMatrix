
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
			     &aux
			       (dtype :float) ;;:tmp
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

      (labels ((create-buckets (points ids bucket-id
				&aux
				  (sumx (if ids
					    (%sum points :axis 0)))
				  (sumx2 (if ids
					     (%sum (%square points) :axis 0))))
		 (make-bucket :d (bucket-d bucket)
			      :point-ids points ;; array?
			      :sumx sumx
			      :sumx2 sumx2
			      :bucket-id bucket-id))
	       (%cmp> (matrix
		       scalar
		       &aux
			 (N (car (matrix-visible-shape matrix)))
			 (result (loop repeat N collect nil)))
		 "matrix = [N, 1]"
		 (call-with-visible-area
		  matrix
		  #'(lambda (view)
		      (with-view-object (index view :absolute i)
			(setf (nth i result) (if (> (1d-mat-aref matrix index) val)
						 t
						 nil)))))
		 result))
	(let* ((xd (view x `(:indices ,@transition-states dim) t))
	       (mask (%cmp> (view xd t dim) val)) ; %cmp > val
	       (not-mask mask) ;; %lognot??? 
	       (x0 (%copy (view x not-mask t))) ;; :tflist
	       (x1 (%copy (view x mask t))) ;; view: enhancement: :tfvalues t nil t ... -> :indices
	       (ids0 (view transition-states not-mask)) ;; they're list.
	       (ids1 (view transition-states mask)))


	  (prog1
	      (values (create-buckets x0 ids0 id0)
		      (create-buckets x1 ids1 id1))
	    (free-mat x0)
	    (free-mat x1)))))))

(defmethod optimal-split-val ((bucket MSBucket) x dim)
  "Returns the split vals j"

  (cond
    ((or (< (bucket-n bucket) 2)
	 (null (bucket-point-ids bucket)))
     (values 0 0))
    (T
     (let ((my-idx (bucket-point-ids bucket)))
       (compute-optimal-split-val
	(view x my-idx t)
	dim)))))

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

(defmethod bucket-loss ((bucket MSBucket))
  (let ((loss (%sumup (col-sum-sqs bucket))))
    (if (> loss 0)
	loss
	0.0)))


;; MultiSplit = [split-dim value-threshold alpha beta]
(defstruct MultiSplit
  (split-dim 0 :type index)
  threshold
  alpha
  beta)

(defmethod preprocess-x ((msplit MultiSplit) x)
  "y = alpha*x + beta element-wise, and destructively"
  (let ((x x))
    (unless (null (multisplit-beta msplit))
      (setq x (%scalar-add x (- (multisplit-beta msplit)))))

    (unless (null (multisplit-alpha msplit))
      (setq x (%scalar-mul x (multisplit-alpha msplit))))

    x))





;; Todo Benchmark
(defun cumsse-cols (x
		    &aux
		      (N (car (matrix-visible-shape x)))
		      (D (second (matrix-visible-shape x)))
		      (dtype (matrix-dtype x)))
  "Computes SSE (Sum of Square Errors) column-wise.
Input: X [N D]
Output: Cumsses [N D]"
  (declare (optimize (speed 3))
	   (type index N D))
  (let ((cumsses (matrix `(,N ,D) :dtype dtype))
	(cumX-cols (matrix `(1 ,D) :dtype dtype))
	(cumx2-cols (matrix `(1 ,D) :dtype dtype)))


    (dotimes (j D)
      (with-views ((cxc cumX-cols j t)
		   (cxc2 cumX2-cols j t)
		   (x* x 0 j))
	(%move x* cxc)
	(%move x* cxc2)
	(%square cxc2)))

    (let ((sqarea))
      (loop for i fixnum upfrom 0 below N
	    do (let ((lr (/ (1+ i))))
		 (dotimes (j D)
		   (with-views ((cs cumsses i j)
				(cxc cumX-cols j t)
				(cxc2 cumX2-cols j t)
				(x* x i j))
		     (%adds cxc x*)
		     (%adds cxc2 (%square (if sqarea
					      (progn
						(%move x* sqarea))
					      (progn
						(setq sqarea (%copy x*))
						sqarea))))
		     (let* ((meanX (%scalar-mul cxc lr))
			    (mx (%muls meanX cxc))
			    (mx (%scalar-mul mx -1.0)))
		       (%move cxc2 cs)
		       (%adds cs mx))))))
      (free-mat cumX-cols)
      (free-mat cumx2-cols)
      (if sqarea
	  (free-mat sqarea))
      cumsses)))

(defun compute-optimal-split-val (x dim)
  "Given x and dim, computes a optimal split-values.

x - One of prototypes. [num_idxs, D]"
  (let* ((N (car (matrix-visible-shape X)))
 	 (sort-idxs (argsort (convert-into-lisp-array (view x t dim) :freep nil)))
	 (sort-idxs-rev      (reverse sort-idxs))
	 (sses-head          (cumsse-cols (view x `(:indices ,@sort-idxs-rev) t)))
	 (sses-tail-reversed (cumsse-cols (view x `(:indices ,@sort-idxs) t)))
	 (sses sses-head)
	 (last-index  (car (matrix-visible-shape sses-head)))
	 (indices (loop for i downfrom last-index to 0
			collect i)))

    (%adds (view sses `(0 ,(1- last-index)) t)
	   (view sses-tail-reversed `(:indices ,@indices) t))

    (let* ((sses (%sum sses :axis 1))
	   (best-idx (nth 0 (argsort (convert-into-lisp-array sses :freep nil) :test #'<)))
	   (next-idx (min (1- N) (1+ best-idx)))
	   (col (%copy (view x t dim)))
	   (best-col (view col (nth best-idx sort-idxs) t))
	   (next-col (%copy (view col (nth next-idx sort-idxs) t))))

      (%adds best-col next-col)
      (%scalar-mul best-col 0.5)
      
      (free-mat next-col)
      (free-mat sses-head)
      (free-mat sses-tail-reversed)
      (free-mat sses)

      (values best-col (view sses best-idx t)))))

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
      ;;result
      (butlast result)))) ;; Fixme: Last Result is invaild

(defun argsort (array &key (test #'>))
  (declare (optimize (speed 3))
	   (type function test)
	   (type (simple-array t (*)) array))
  (mapcar #'second
          (stable-sort
           (loop
             for index fixnum from 0
             for element-i fixnum upfrom 0 below (array-total-size array)
             collect (list (aref array element-i) index))
	   test
           :key #'first)))

;; FixMe: Refactor it since the code is crazy...
(defun learn-binary-tree-splits (X
				 x-orig
				 N
				 &key
				   (nsplits 4) ;; Levels of resuting binary hash tree. (4 is the best).
				   (need-prototypes nil)
				   (learn-quantize-params t))
  "Training the given prototype, X and X-orig
  X, X-orig = [C, D]
Algorithm 2. Adding The Next Levels to The Tree

learn-quantize-params : set t if X's dtype isn't uint_8. (restore 8bit aggregations)
scal-by, offset: alpha, beta which corresponds to y = alpha*x + beta."

  ;; Assert:: (> nsplits 4)
  
  (let* ((D (second (matrix-visible-shape X)))
	 (X-copy (%copy X)) ;; X-copy is shared by every buckets.
	 (X-square (%square (%copy X)))
	 (X-orig (%copy x-orig)) ;; X-copy is shared too?
	 (buckets (list
		   ;; Creates the toplevel of bucket.
		   ;; point-ids = 0~N because it has the original X.
		   ;; Semantics: B(1, 0) -> B(2, 0), B(2, 1), ..., B(2, N)
		   (make-bucket
		    :sumx (%sum x-copy :axis 0)
		    :sumx2 (%sum x-square :axis 0)
		    :point-ids (loop for i fixnum upfrom 0 below N
				     collect i))))
	 (splits)
	 
	 ;; The storerooms of losses (should be list?)
	 (col-losses (matrix `(1 ,D) :dtype :float))
	 
	 (offset 0.0)
	 (scal-by 1.0)
	 ;; X' = alpha*X + offset
	 (x (%scalar-add (%scalar-mul X-copy scal-by) offset)))

    ;; Loop for splits times.
    (loop repeat nsplits
	  do (progn
	       ;; be list?
	       (%fill col-losses 0.0)
	       
	       ;; heuristic = bucket_sse
	       (dolist (buck buckets)
		 (let ((loss (col-sum-sqs buck)))
		   (%adds col-losses loss)
		   (free-mat loss)))

	       ;; dim-order -> [Largest Loss ... Smallest Loss]
	       (let* ((dim-order (the list (argsort (convert-into-lisp-array col-losses :freep nil))))
		      (all-split-vals)
		      (dim-size (length dim-order))
		      (total-losses (matrix `(1 ,dim-size) :dtype (matrix-dtype X))))

		 (labels ((previous-min-losses (d)
			    "Find out minimize value in the range of total-losses[:d]"
			    (let ((idx  (car (argsort (convert-into-lisp-array
						       (view total-losses t `(0 ,d))
						       :freep nil)
						      :test #'<))))
			      (1d-mat-aref total-losses idx))))

		   ;; Todo: Refactor...
		   ;; Loop for (length dim-order) times.
		   (loop for d fixnum upfrom 0
			 for dim fixnum in dim-order
			 do (let ((split-vals))
			      (loop
				named bucket-training-iter
				for b in buckets
				do (multiple-value-bind (val loss) (optimal-split-val b X dim)
				     (%scalar-add (view total-losses t d) loss)
				     (when (and (> d 0)
						(> (1d-mat-aref total-losses d)
						    (previous-min-losses d)))
				       ;; Early Stopping
				       (return-from bucket-training-iter))
				     (push val split-vals)))
			      (push (reverse split-vals) all-split-vals))))
	       ;; The code below belongs to 1th iter.
		 (let* ((best-trying-dim (car (argsort (convert-into-lisp-array total-losses :freep nil) :test #'<)))
			(best-dim (nth best-trying-dim dim-order))
			(use-split-vals (nth best-trying-dim all-split-vals))
			(split (make-multisplit
				:split-dim best-dim
				:threshold use-split-vals
				:alpha nil
				:beta  nil)))

		   (if learn-quantize-params
		       (progn

			 )
		       (with-slots ((alpha alpha) (beta beta)) split
			 (setf alpha scal-by)
			 (setf beta offset)))

		   (push split splits)

		   (let ((new-buckets (loop for i fixnum upfrom 0
					    for b in buckets
					    nconc (let ((val (nth i use-split-vals)))
						    (bucket-split b :dim best-dim :val val :x-orig x-orig)))))

		     ;(mapc #'destroy-bucket buckets)
		     (setq buckets new-buckets))))))
    (let ((loss (loop for b in buckets
		      sum (bucket-loss b))))
      (if need-prototypes
	  (progn

	    )
	  (progn
	    (values splits loss buckets))))))


(defun init-and-learn-mithral (X
			       C
			       ncodebooks
			       &key
				 (K 16))
  "Creates All Prototypes from C, and learning hash-function g(a).
In the maddness paper, the single-float matrix X is compressed into lower bit's matrix, 8bit and restored as it was. however this implementation exceptes to X uint16_t.

X = [N D] (To be optimized)
y = [D M] (To be multiplied)"
  (declare (type matrix x)
	   (type index C ncodebooks)) ;; X.dtype = :uint16_t

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
	      (learn-binary-tree-splits use-x-error use-x-orig N :need-prototypes nil)

	    (print msplits)
	    (print protos)
	    (print buckets)
	    ))))

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


