
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.maddness
  (:use :cl :cl-xmatrix)
  (:export
   #:init-and-learn-mithral))

(in-package :cl-xmatrix.amm.maddness)

;; This is the my reimplementation of Maddness
;; https://arxiv.org/pdf/2106.10860.pdf
;; Ref: (Bolt, the author's implementation) https://github.com/dblalock/bolt/blob/e7726a4c165cc45ac117e9eabd8761013a26640e/experiments/python/clusterize.py#L1362
;;      (Halutmatmul) https://github.com/joennlae/halutmatmul

;; Wrappers for Mithral's C++ API



#|(defcfun ("mithral_encode_int16_t" mithral-encode) :void
  (X-pointer (:pointer :uint16))
  (nrows :int64)
  (ncols :int)
  (splitdims-pointer (:pointer :uint32))
  (all-splitvals-pointer (:pointer :uint8))
  (shifts (:pointer :uint8))
  (offsets (:pointer :int16))
  (ncodebooks :int)
  (out-pointer (:pointer :uint8)))
|#


;; TODO: OPTIMIZE

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

;; Memo

(deftype index () `(or fixnum))

(defstruct (MSBucket ;; Bucket
	    (:conc-name bucket-))
  (point-ids nil :type list) ;; the list of indices that could be branch's destination.
  (n nil :type index)
  (id nil :type fixnum) ;; split-index j1 ... j4
  (adjustable nil :type boolean)
  (d nil :type fixnum)
  (sumx nil :type matrix)
  (sumx2 nil :type matrix))

(declaim (inline make-bucket))
(defun make-bucket (&key
		      (d nil)
		      (n 0)
		      (sumx  nil)
		      (sumx2 nil)
		      (point-ids nil)
		      (bucket-id 0)
		      (adjustable nil)
		      (dtype :float))
  (declare (optimize (speed 3) (safety 0))
	   (type (or null fixnum) d)
	   (type (or null matrix) sumx sumx2)
	   (type list point-ids)
	   (type fixnum bucket-id n)
	   (type boolean adjustable))

  
  (if (and (null point-ids)
	   (not (= N 0)))
      (error "Assertion Failed with (not (= N 0)) and (Null Point-ids)"))

  (let* ((n (length point-ids))
	 (d (cond ;; Update D
	      ((and (or (null d) ;; is bucket a top?
			(< d 1)) 
		    (not (null sumx)))
	       ;; => Bucket posses the original X.
	       (apply #'* (shape sumx)))
	      ((and (or (null d) ;; is bucket a top?
			(< d 1))
		    (not (null sumx2)))
	       ;; => Bucket possess the original X.
	       (apply #'* (shape sumx2)))
	      (t
	       (if (null d)
		   (error "Assertion Failed with (!= n 0)")
		   d))))
	 (sumx  (or sumx  (matrix `(1 ,D) :dtype dtype)))
	 (sumx2 (or sumx2 (matrix `(1 ,D) :dtype dtype))))
    (make-msbucket :d d
		   :n n
		   :sumx sumx
		   :sumx2 sumx2
		   :point-ids point-ids
		   :id bucket-id
		   :adjustable adjustable)))

(defmethod destroy-bucket ((bucket MSBucket))
  "Memfree the bucket's non-gc-able matrices"
  (with-slots ((sumx sumx) (sumx2 sumx2)) bucket
    (free-mat sumx)
    (free-mat sumx2)))

;; Not tested (maybe unused currently)
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
  (declare (optimize (speed 3) (safety 0))
	   (inline make-bucket))
  ;; To fix: don't create copy! the toplevel matrix's view is enough.
  (make-bucket
   :sumx      (bucket-sumx bucket)
   :sumx2     (bucket-sumx2 bucket)
   :point-ids (copy-list (bucket-point-ids bucket))
   :bucket-id bucket-id))

;; Optimize: Type Convertions, between List <-> Matrix
(defmethod bucket-split ((bucket MSBucket)
			 x
			 &key
			   (dim nil)
			   (val nil))
  "Given val (threshold v?), splits the bucket, and increment: node-level t+=1.
B(t, ?) -> B(t+1, ?), B(t+1, ??), ...

Algorithm 2 Adding The Next Level to The Hashing Tree.

Bucket-Split: B(A) -> B(id0), B(id1)"
  (declare (optimize (speed 3))
	   (type number val)
	   (inline make-bucket))

  ;; X = [N, C]
  
  ;; Algorithm 1. MaddnessHash
  (let* ((id0 (* 2 (bucket-id bucket)))
 	 (id1 (+ id0 1)))
    (declare (type index id0 id1))

    (when (or (null x) (< (bucket-n bucket) 2))
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

      ;; Create masks for point-idx rows
      ;; And Compares the row[dim] with val.
      ;; dim and val is a parameter to be optimized.

      (labels ((create-buckets (points ids bucket-id
				&aux
				  (sumx (if ids
					     (%sum (%copy points) :axis 0)))
				  (sumx2 (if ids
					     (%sum (%square points) :axis 0))))
		 (make-bucket :d (bucket-d bucket)
			      :point-ids ids
			      :sumx sumx
			      :sumx2 sumx2
			      :bucket-id bucket-id))
	       (%cmp> (matrix scalar)
		 "matrix = [N, 1]. Result=List"
		 (%satisfies matrix
			     #'(lambda (x)
				 ;; with-typevar?
				 ;; may error when dtype!=:float
				 (declare (type single-float x scalar))
				 (> x scalar))))
	       (next-point-ids (list tf-list)
		 (loop for l in list
		       for count fixnum upfrom 0 below (car (shape tf-list))
		       if (= (the single-float
				  (1d-mat-aref tf-list count))
			     1.0)
			 collect l)))
	;; Here, remains to be optimized.
	(let* ((xd (view x `(:indices ,@transition-states) dim))
	       (mask     (%cmp> xd val)) ;; %cmp > val
	       (not-mask (%filter mask #'(lambda (x)
					   ;; Fixme: x's dtype is unknown
					   (if (= (the single-float x) 1.0)
					       0.0
					       1.0))))
	       (x0 (%copy (view x `(:tflist ,not-mask) t)))
	       (x1 (%copy (view x `(:tflist ,mask)     t)))
	       
	       (ids0 (next-point-ids transition-states not-mask))
	       (ids1 (next-point-ids transition-states mask)))

	  (free-mat mask)
	  (free-mat not-mask)

	  (values (create-buckets x0 ids0 id0)
		  (create-buckets x1 ids1 id1)))))))

(declaim (ftype (function (MSBucket matrix index) (values (or null matrix) (or null matrix))) optimal-split-val))
(defmethod optimal-split-val ((bucket MSBucket) x dim)
  "Computes `optimal-split-threshold` (in 4.2 Hash Function Family).

Input: x  - a view of X-orig
      (bucket matrix index) 
Return:
      (values matrix matrix)
Memo: Spelling Inconsistency -> threshold and val."
  (declare (optimize (speed 3)))
  (cond
    ((or (< (bucket-n bucket) 2)
	 (null (bucket-point-ids bucket)))
     (values nil nil))
    (T
     (let ((my-idx (bucket-point-ids bucket)))
       (compute-optimal-split-val
	(view x `(:indices ,@my-idx) t)
	dim)))))

;; Computes losses
(defmethod col-variances ((bucket MSBucket))
  (declare (optimize (speed 3)))
  (if (< (bucket-n bucket) 1)
      (matrix `(1 ,(bucket-d bucket)) :dtype :float) ;; Fixme: dtype is unknown
      (let ((ex2 (%scalar-div
		  (%copy (bucket-sumx2 bucket))
		  (bucket-n bucket)))
	    (ex (%scalar-div
		 (%copy (bucket-sumx bucket))
		 (bucket-n bucket))))
	(%adds ex2 (%scalar-mul (%square ex) -1.0)))))

(defmethod col-means ((bucket MSBucket))
  (declare (optimize (speed 3)))
  (%scalar-div (%copy (bucket-sumx bucket)) (max 1 (bucket-n bucket))))

(defmethod col-sum-sqs ((bucket MSBucket))
  (declare (optimize (speed 3)))
  (%scalar-mul (col-variances bucket) (bucket-n bucket)))

(defmethod bucket-loss ((bucket MSBucket))
  (declare (optimize (speed 3)))
  (let ((loss (%sumup (col-sum-sqs bucket))))
    (declare (type single-float loss))
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
  (declare (optimize (speed 3)))
  (let ((x x))
    (unless (null (multisplit-beta msplit))
      (setq x (%scalar-sub x (multisplit-beta msplit))))

    (unless (null (multisplit-alpha msplit))
      (setq x (%scalar-mul x (multisplit-alpha msplit))))
    x))


;;measuring PROFILE overhead..done
;;  seconds  |     gc     |   consed   |  calls  |  sec/call  |  name  
;;----------------------------------------------------------
;;     0.324 |      0.238 | 11,014,800 |      28 |   0.011564 | CL-XMATRIX::ALLOCATE-MAT
;;     0.013 |      0.000 |  1,603,808 |   6,314 |   0.000002 | CL-XMATRIX:CALL-WITH-VISIBLE-AREA
;;     0.003 |      0.000 |          0 |  23,807 |   0.000000 | CL-XMATRIX::INDEX-P
;;     0.002 |      0.000 |          0 |  10,808 |   0.000000 | CL-XMATRIX::TRANSCRIPT-VIEW
;;     0.001 |      0.000 |          0 |  10,871 |   0.000000 | CL-XMATRIX:SHAPE
;;     0.001 |      0.000 |          0 |   3,654 |   0.000000 | CL-XMATRIX::DIMS
;;     0.001 |      0.000 |          0 |   3,710 |   0.000000 | CL-XMATRIX::VIEW-STARTINDEX
;;     0.001 |      0.000 |          0 |   3,710 |   0.000000 | CL-XMATRIX::VIEW-ENDINDEX
;;     0.001 |      0.000 |          0 |   1,827 |   0.000000 | CL-XMATRIX::SUBSCRIPT-P
;;     0.001 |      0.000 |          0 |   2,688 |   0.000000 | CL-XMATRIX::FP32-ADD
;;     0.001 |      0.000 |    357,632 |   1,827 |   0.000000 | CL-XMATRIX::PARSE-BROADCAST-SUBSCRIPTS
;;     0.000 |      0.000 |          0 |   1,827 |   0.000000 | CL-XMATRIX::PARSE-AND-REPLACE-TFLIST-SUBSCRIPTS
;;     0.000 |      0.000 |          0 |   1,792 |   0.000000 | CL-XMATRIX::FP32-SCALAR-MUL

;; Todo Benchmark
;; 目標 0.0000406

;;0.025 |      0.000 | 3,998,976 |     15 |   0.001677 | ALLOCATE-MAT
;;0.003 |      0.000 | 1,113,488 |  4,495 |   0.000001 | CALL-WITH-VISIBLE-AREA
;; 0.000 |      0.000 |   195,072 |  1,295 |   0.000000 | PARSE-BROADCAST-SUBSCRIPTS

(defun cumsse-cols (x
		    &aux
		      (N (car (shape x)))
		      (D (second (shape x)))
		      (dtype (dtype x)))
  "Computes SSE (Sum of Square Errors) column-wise.
Input: X [N D]
Output: Cumsses [N D]"
  (declare (optimize (speed 3) (safety 0))
	   (type index N D)
	   (type matrix x))
  ;; To reduce alloc-mat: -> with-cache
  (let ((cumsses    (matrix `(,N ,D) :dtype dtype))
	(cumX-cols  (matrix `(1 ,D)  :dtype dtype))
	(cumx2-cols (matrix `(1 ,D)  :dtype dtype)))

    ;; cumsses ... each N's Loss

    ;; First Step (n=0), (for initializing)
    ;; ここ, 0.000059 sec 100.00% CPU.
    ;;       0.0000406   (NUMBA)
    ;;       0.00003194  (NUMBA)
    ;;       0.000005    (call-with-visible-area) * 1

    ;; 0.000013~0.000009 sec

    ;; (sb-profile:profile "CL-XMATRIX")

    ;; Numba n=1000
    ;; 0.05 sec
    ;; cl-xmatrix n=1000
    ;; 0.087 sec.

    (with-views ((cxc cumX-cols 0 t)
		 (cxc2 cumX2-cols 0 t)
		 (x* x 0 t)
		 (cs cumsses 0 t))
      (%move x* cxc)
      (%move x* cxc2)
      (%square cxc2)
      (dotimes (i N)
	(let ((lr (/ (+ 2.0 i))))
	  
	  (%adds cumX-cols x*)
	  (%adds cumX2-cols x*)
	  (let* ((meanX (%scalar-mul cumX-cols lr))
		 (mx    (%muls meanX cumX-cols))
		 (mx    (%scalar-mul mx -1.0)))
	    (%move cumX2-cols cs)
	    (%adds cs mx)))
	(incf-offsets! x* 1 0)
	(incf-offsets! cs 1 0))
      (reset-offsets! x*)
      (reset-offsets! cs))

    ;;(sb-profile:report)
    ;;(sb-profile:unprofile "CL-XMATRIX")
    
    (free-mat cumX-cols)
    (free-mat cumx2-cols)
    cumsses))

;; This function is O(N)
(declaim (ftype (function (matrix index) (values matrix matrix)) compute-optimal-split-val))
(defun compute-optimal-split-val (x dim)
  "Given x and dim, computes a optimal split-values.

x - One of prototypes. [num_idxs, D]"
  (declare (optimize (speed 3))
	   (type fixnum dim)
	   (type matrix x))
  (let* ((N (car (shape X)))
 	 (sort-idxs (argsort (convert-into-lisp-array (view x t dim))))
	 (sort-idxs-rev      (reverse sort-idxs))
	 (sses-head          (cumsse-cols (view x `(:indices ,@sort-idxs-rev) t)))
	 (sses-tail-reversed (cumsse-cols (view x `(:indices ,@sort-idxs) t)))
	 (sses sses-head)
	 (last-index  (the index (car (shape sses-head))))
	 (indices (loop for i fixnum downfrom (1- last-index) to 0 collect i)))
    (declare (type index N))
    
    (%adds (view sses `(0 ,last-index) t)
	   (view sses-tail-reversed `(:indices ,@indices) t))

    (let* ((sses (%sum sses :axis 1))
	   (best-idx (the index (nth 0 (argsort (convert-into-lisp-array sses :freep nil) :test #'<))))
	   (next-idx (min (the index (1- N)) (the index (1+ best-idx))))
	   (col (%copy (view x t dim)))
	   (best-col (view col (nth best-idx sort-idxs) t))
	   (next-col (%copy (view col (nth next-idx sort-idxs) t))))

      (%adds best-col next-col)
      (%scalar-mul best-col 0.5)
      
      ;(free-mat next-col)
      ;(free-mat sses-head)
      ;(free-mat sses-tail-reversed)

      ;; matrix matrix
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
      (print result)))) ;; Fixme: Last Result is invaild

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

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
				   (learn-quantize-params nil))
  "Training the given prototype, X and X-orig
  X, X-orig = [C, D]
Algorithm 2. Adding The Next Levels to The Tree

learn-quantize-params : set t if X's dtype isn't uint_8. (restore 8bit aggregations)
scal-by, offset: alpha, beta which corresponds to y = alpha*x + beta."

  ;; Assert:: (> nsplits 4)

  (declare (optimize (speed 3))
	   (type fixnum nsplits N))
  (let* ((D (second (shape X)))
	 (X-copy     (%copy X)) ;; X-copy is shared by every buckets.
	 (X-square   (%square (%copy X)))
	 (buckets (list
		   ;; Creates the toplevel of bucket.
		   ;; point-ids = 0~N because it has the original X.
		   ;; Semantics: B(1, 0) -> B(2, 0), B(2, 1), ..., B(2, N)
		   (make-bucket
		    :sumx  (%sum x-copy :axis 0)
		    :sumx2 (%sum x-square :axis 0)
		    :point-ids (loop for i fixnum upfrom 0 below N
				     collect i))))
	 (splits)
	 
	 (col-losses (matrix `(1 ,D) :dtype :float))
	 
	 (offset 0.0)
	 (scal-by 1.0)
	 ;; X' = alpha*X + offset
	 (x (%scalar-add (%scalar-mul X-copy scal-by) offset)))
    (declare (type list splits))

    ;; Loop for splits times.
    (loop repeat nsplits
	  do (progn
	       ;; be list?
	       (%fill col-losses 0.0)
	       
	       ;; heuristic = bucket_sse
	       (with-pointer-barricade
		 (dolist (buck buckets)
		   (%adds col-losses (col-sum-sqs buck))))

	       ;; dim-order -> [Largest Loss ... Smallest Loss]
	       (let* ((dim-order (the list (argsort (convert-into-lisp-array col-losses :freep nil))))
		      (all-split-vals)
		      (dim-size (length dim-order))
		      (total-losses (matrix `(1 ,dim-size) :dtype (dtype X))))
		 (declare (type matrix total-losses)
			  (type list dim-order all-split-vals)
			  (type index dim-size))

		 (labels ((previous-min-losses (d)
			    "Find out minimize value in the range of total-losses[:d]"
			    (declare (type index d))
			    (let ((idx  (car (argsort (convert-into-lisp-array
						       (view total-losses t `(0 ,d))
						       :freep nil)
						      :test #'<))))
			      (1d-mat-aref total-losses idx))))

		   ;; Todo: Refactor...
		   ;; Loop for (length dim-order) times.
		   (loop for d fixnum upfrom 0
			 for dim fixnum in dim-order
			 do (let ((split-vals nil))
			      (declare (type list split-vals))
			      (loop
				named bucket-training-iter
				for b in buckets
				do (multiple-value-bind (val loss) (optimal-split-val b X dim)
				     (let ((val (or val (matrix `(1 1))))
					   (loss (if loss
						     (cl-xmatrix::1d-mat-aref loss 0)
						     0.0)))
				     (%scalar-add (view total-losses t d) loss)
				     (when (and (>= d 1)
						(>= (the single-float
							 (1d-mat-aref total-losses d))
						    (the single-float (previous-min-losses d))))
				       ;; Early Stopping
				       (return-from bucket-training-iter))
				     (push val split-vals))))
			      (when (and split-vals buckets)
				(push (reverse split-vals) all-split-vals)))))
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
			 (error "Unimplemented"))
		       (with-slots ((alpha alpha) (beta beta)) split
			 (setf alpha scal-by)
			 (setf beta offset)))

		   (push split splits)

		   (let ((new-buckets (loop for i fixnum upfrom 0
					    for b in buckets
					    collect (let ((val (1d-mat-aref (nth i use-split-vals) 0)))
						    (bucket-split b x :dim best-dim :val val)))))
		     
		     ;(mapc #'destroy-bucket buckets)
		     (setq buckets (flatten new-buckets)))))))
    (let ((loss (loop for b in buckets sum (the single-float (bucket-loss b)))))
      (if need-prototypes
	  (progn
	    (error "Unimplemented")
	    )
	  (progn
	    (values (reverse splits) loss buckets))))))


(defun init-and-learn-mithral (X
			       C
			       ncodebooks
			       &key
				 (K 16))
  "Creates All Prototypes from C, and learning hash-function g(a).
In the maddness paper, the single-float matrix X is compressed into lower bit's matrix, 8bit and restored as it was. however this implementation exceptes to X uint16_t.

X = [N D] (To be optimized)
y = [D M] (To be multiplied)"
  (declare (optimize (speed 3))
	   (type matrix x)
	   (type index C ncodebooks)
	   (ignore ncodebooks)) ;; X.dtype = :uint16_t

  (assert (= 2 (length (the list (shape X))))
	  (x)
	  "Assertion Failed with X.dims == 2 ~a"
	  (shape x))

  (let* ((x-error (%copy X)) ;; Valid Dataset (DONT FORGET: memfree x-error)
	 (x-orig x)          ;; Training Dataset (minimize ||a-a'||)
	 (N (car (shape X)))
	 (D (second (shape X)))
	 (all-prototypes (matrix `(,C ,K ,D) :dtype (cl-xmatrix::matrix-dtype X)))
	 (all-splits nil) ;; The Result
	 (all-buckets nil) ;; Tmp List
	 ;; indices of disjoints based on C are needed when training KMeans. (j)
	 (pq-idxs (create-codebook-idxs D C :start-or-end :start)))
    ;; Fix: pq-idxs[1]'s last index.
    (declare (type list pq-idxs))
    (dotimes (cth (length pq-idxs)) ;; Applying to each prototypes.
      (let ((cth-idx (nth cth pq-idxs)))
	(print cth-idx)
	(with-views ((use-x-error x-error t `(,(first cth-idx)
					      ,(second cth-idx)))
		     (use-x-orig  x-orig  t `(,(first cth-idx)
					      ,(second cth-idx))))
	  ;; Iteraiton: [100, D] -> [100, 0~4], [100, 4~8] ...
	  
	  (multiple-value-bind (msplits loss buckets)
	      (learn-binary-tree-splits use-x-error use-x-orig N :need-prototypes nil)
	    (format t "~%Loss :~a " loss)

	    (loop for s in msplits
		  do (setf (multisplit-split-dim s)
			   (+ (the index (first cth-idx))
			      (multisplit-split-dim s))))
	    
	    
	    (push msplits all-splits)
	    (push buckets all-buckets)

		    
            ;; update residuals and store prototypes
            ;; idxs = IDs that were look at for current codebook
            ;; buck.point_ids = rows that landed in certain K
            ;;   [    0     5    21 ... 99950 99979 99999] (N=100000)
            ;; X_error = is here still the A input
            ;; remove centroid from all the points that lie in a certain codebook
            ;; set prototype value

	    (let ((centroids (matrix `(1 ,D) :dtype (dtype X)))
		  (idxs (loop for i fixnum
			      upfrom (first cth-idx)
				below (second cth-idx)
			      collect i)))
	      (loop for b fixnum upfrom 0
		    for bucket in buckets
		    if (bucket-point-ids bucket)
		      do (let ((mean (col-means bucket))
			       (bids (bucket-point-ids bucket)))
			   (%fill centroids 0.0)
			   ;; Centroids = [1 D]
			   ;; X-error   = [N D]
			   (with-views ((c* centroids 0 `(:indices ,@idxs))
					(xerr* use-X-error `(:indices ,@bids) t))
			     (loop for i fixnum upfrom 0
				   for id in idxs
				   do (with-views ((c* centroids 0 id)
						   (m* mean 0 i))
					(%move m* c*)))
			     
			     (dotimes (m (the index (car (shape xerr*))))
			       (with-view (e* xerr* m)
				 (%subs e* c*)))

			     (with-view (protos* all-prototypes cth b t)
			       ;; Reshape Centroids
			       ;; Fixme: Copying
			       (let ((centroids-res (cl-xmatrix::reshape
						     (%copy centroids)
						     `(1 ,@(shape centroids)))))
				 (%move centroids-res protos*))))))
	      (free-mat centroids)
	      
	      ;; Todo: Ram Usage
	      )))))
    (values x-error (reverse all-splits) all-prototypes (reverse all-buckets))))

;; Fix IT
(defun apply-hash-function (X splits &aux (N (car (matrix-visible-shape X))))
  "splits ... list, consisted of MultiSplits"
  (declare (type matrix X)
	   (type list splits))
  (let ((nsplits (length splits)))
    (unless (>= nsplits 1)
      (error "Assertion Failed with (>= nsplits 1)"))
    (labels ((%cmp> (matrix
		     scalar
		     &aux
		       (N (car (matrix-visible-shape matrix)))
		       (result (loop repeat N collect nil)))
	       "matrix = [N, 1]"
	       (call-with-visible-area
		matrix
		#'(lambda (view)
		    (with-view-object (index view :absolute i)
		      (setf (nth i result) (if (> (1d-mat-aref matrix index) scalar)
					       t
					       nil)))))
	       result))
      (let ((group-ids (loop repeat N collect 0)))
	(loop for i upfrom 0 below nsplits
	      do (let* ((split (nth i splits))
			(vals (loop for i in group-ids
				    collect (nth i (multisplit-threshold split))))
			(indicators (preprocess-x
				     split
				     (%copy (view X t `(:indices ,@(multisplit-split-dim split))))))
			(indicators (%cmp> indicators (car vals)))) ;; ??? Fixme: fix here.
		   (loop for i upfrom 0 below (length group-ids)
			 do (progn
			      (setf (nth i group-ids)
				    (+
				     (* (nth i group-ids) 2)
				     (nth i indicators)))))))
	group-ids))))

(defun mithral-encode ()
  ;; call CFFI and SIMDlize
  )

(defun learn-proto-and-hash-function (X C
				      &key
					(K 16)
					(verbose t)
				      &aux
					(X-copy (%copy X))
					(size (apply #'* (matrix-visible-shape X)))
					(delta 1e-7))
  (multiple-value-bind (x-error all-splits all-prototypes _)
      (init-and-learn-mithral X C 4 :K K)
    (declare (ignore _))

    (let ((msv-orig  (/ (%sumup (%square X-copy)) size))
	  (mse-error (+ delta (/ (%sumup (%square X-error)) size))))
      (when verbose
	(format t "X-error-mse / X mean squared value: ~a"
		(/ msv-orig mse-error)))


      )))

;; Note: Compute on INT8



  
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
