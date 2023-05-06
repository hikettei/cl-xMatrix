
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.maddness
  (:use :cl :cl-xmatrix :cffi)
  (:export
   ))

(in-package :cl-xmatrix.amm.maddness)

;; This is the my reimplementation of Maddness

;; TODO: Comments for readers
;; TODO: Construct Prototypes
;; TODO: 8bit Aggregations
;; TODO: Optimizing Prototypes Ridge Regression
;; TODO: Scan
;; 二分木の学習がうまくいかない？
;; MaddnessHashのLossが小さい...
;; AlphaとBetaのパラメーターがおかしい
;; 変な軸でsum取ってる気がする, the larger N -> more likelihood to make overflow.
(deftype index () `(or fixnum))

(defmacro with-bucket-clusters ((idx-var bucket-var tree-level top-bucket)
				 &body body
				 &aux (bucket-id (gensym)))
  `(let ((bucket-idx-counter 0))
     (declare (type fixnum bucket-idx-counter ,tree-level))
     (labels ((explore-bucket (,bucket-id)
		(if (= (bucket-tree-level ,bucket-id) ,tree-level)
		    (let ((,idx-var    bucket-idx-counter)
			  (,bucket-var ,bucket-id))
		      (declare (ignorable ,idx-var ,bucket-var))
		      (incf bucket-idx-counter 1)
		      ,@body)
		    (let ((nodes (bucket-next-nodes ,bucket-id)))
		      (explore-bucket (car nodes))
		      (explore-bucket (cdr nodes))))))
       (explore-bucket ,top-bucket))))

(defun meanup (matrix)
  (declare (optimize (speed 3) (safety 0)))
  (/ (the single-float (%sumup matrix))
     (the fixnum (apply #'* (shape matrix)))))

(defun learn-prototypes-and-hash-function (X C &key (nsplits 4) (verbose t) (optimize-protos t))
  (declare (optimize (speed 3))
	   (type matrix X)
	   (type fixnum C))

  (with-caches ((X-error (shape X) :dtype (dtype X))
		(X-tmp   (shape X) :dtype (dtype X)))
    (%move X X-error)

    (multiple-value-bind (buckets protos)
	(init-and-learn-offline X-error C :nsplits nsplits :verbose verbose)

      (when verbose
	(let ((mse-orig
		(progn
		  (%move X X-tmp)
		  (%square X-tmp)
		  (meanup X-tmp)))
	      (mse-error
	 	(progn
		  (%move X-error X-tmp)
		  (%square X-tmp)
		  (meanup X-tmp))))
	  (declare (type single-float mse-orig mse-error))

	  (format t "== Report: ====~%MSE-Error / MSE-Orig -> ~a~%MSE-Error -> ~a~%MSE-Orig -> ~a~%"
		  (/ mse-orig mse-error)
		  mse-orig
		  mse-error)))

      ;; Optimizing Prototypes

      (when optimize-protos
        (optimize-protos-with-ridge! protos x x-error buckets nsplits))

      (values buckets protos))))

(defun sparsify-and-int8-a-enc (a-enc K
				&aux
				  (N (car (shape a-enc)))
				  (C (second (shape a-enc)))
				  (D (* C K)))				
    "
    returns X_binary from an encoded Matrix [N, C] vals (0-K)
    to
    [[0 0 0 ... 0 0 0]
     [0 0 1 ... 0 0 0]
     ...
     [0 0 0 ... 0 0 0]
     [0 0 0 ... 1 0 0]
     [0 0 0 ... 0 0 0]]
    "
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum N C D K))
  (let ((out (make-array (* N D) :element-type '(unsigned-byte 256))))
    (declare (type (simple-array (unsigned-byte 256) (*)) out))
    (dotimes (nth N)
      (dotimes (cth C)
	(let* ((code-left (round (the single-float (%sumup (view a-enc nth cth)))))
	       (dim-left  (+ (the fixnum (* K cth)) code-left)))
	  (declare (type fixnum code-left))
	  (setf (aref out (+ (the fixnum (* N nth)) dim-left)) 1))))
    out))

(defun optimize-protos-with-ridge! (protos x x-error buckets nsplits
				    &key
				      (lmd 1.0)
				      (required-loss 0.1)
				      (lr 1e-2)
				    &aux (K (expt 2 nsplits)))
  " minimize loss of |Y - Xw|^2 + alpha * |w|^2
    X is binary in our case -> w without entry in X
    X [N, C * K]
    Y [N, C]
    W [D, C * K] -> W.T [C * K, D] later reshaped to
    [C, K, D] -> prototype dimensons"
  (with-cache (A-enc `(,(car (shape X)) ,K) :place-key :out-cache :dtype :uint8)
    (multiple-value-bind (scales offsets thresholds dims) (flatten-buckets buckets :nsplits nsplits)

      ;; Encoding A
      (maddness-encode-c
       (matrix-vec X)
       (car    (shape X)) ;; N
       (second (shape X)) ;; D
       (matrix-vec dims)
       (matrix-vec thresholds)
       (matrix-vec scales)
       (matrix-vec offsets)
       K
       (matrix-vec A-enc))

      (let ((x-binary (sparsify-and-int8-a-enc a-enc K)))
	;; x-binary = [N D]
	;;(print protos)
	
	(let ((result
		(cl-xmatrix.amm.least-squares:optimize-with-ridge-regression
		 x-binary
		 x-error
		 (shape protos)
		 :alpha lmd
		 :required-loss required-loss
		 :lr lr)))
	  (declare (type cl-waffe:waffetensor result))
	  (let ((result (from-facet
			 (cl-waffe:!shape result)
			 result
			 :direction
			 :foreign-waffe)))
	    (%adds protos result)
	    ;; todo: check how much improvement we got.
	    ))))))

(defun init-and-learn-offline (a-offline ;; a-offline is modified.
			       C
			       &key
				 (all-prototypes-out nil)
				 (nsplits 4)
				 (verbose t)
				 (K 16)
			       &aux
				 (N (car (shape a-offline)))
				 (D (second (shape a-offline))))
  "
The function init-and-learn-offline clusters the prototypes, and then constructs the encoding function g(a).

Assertions:
  1. N must be divided by C (while the original impl doesn't impose it.)
  2. a-offline is a 2d-matrix.

Semantics:
=========================================================================
   D         C      C
  +++       +--    -+-
N +++ =>  N +--  N -+-  <- N*D Matrix is disjointed into N*C Matrix.
  +++       +--,   -+-  ... * (N/D), Binary-Tree-Split is applied into each visible area.
=========================================================================

Input:
  - a-offline The Training Matrix.
  - C fixnum  The Parameter Variable, C.
Return:
  - (values Prototypes List<Buckets> Loss) 
"
  (declare (optimize (speed 3))
	   (type matrix a-offline)
	   (type fixnum N D C K))

  (assert (= (mod D C) 0) nil "Assertion Failed with (= (mod D C) 0). D=~a C=~a" N C)

  ;; (For zenn.dev reader: 初めに処理する行列の形状を決めて、その後Offsetを加算します。) <- atodekesu
  ;; with-view: Cut out the shape of matrix.
  ;; The visible-area is adjusted by modifying offsets.
  ;;    D        D
  ;;   +++    C +--
  ;; N +++ =>   +--
  ;;   +++      +--
  ;;
  ;; Symbols: + ... Visible / - ... Invisible

  ;; all-prototypes (C, K, D)

  (let* ((step (/ D C))
	 (all-prototypes (or all-prototypes-out
			     (matrix `(,C ,K ,D) :dtype (dtype a-offline))
			     )))
    
    (with-views ((a-offline* a-offline t `(0 ,step))
		 (all-prototypes* all-prototypes 0 0 `(0, STEP)))
      ;; subspace = [N, STEP]
      (values
       (loop for i fixnum upfrom 0 below D by step
	     for c fixnum upfrom 0 below C
	     collect (let ((bucket (learn-binary-tree-splits a-offline* STEP :nsplits nsplits :verbose verbose)))
		       (with-cache (centroid `(1 ,STEP) :place-key :centroids)

			 ;; Update X-centroids
			 (with-bucket-clusters (id buck nsplits bucket)

			   (%fill centroid 0.0) ;; fill with col-means
			   (col-means buck a-offline* centroid)

			   (with-views ((a* a-offline* `(:indices ,@(bucket-indices buck)))
					(c* centroid `(:broadcast ,(length (bucket-indices buck)))))
			     (%subs a* c*))

			   (incf-view! all-prototypes* 1 id)
			   (incf-offsets! all-prototypes* c)

			   (%move (cl-xmatrix::reshape centroid `(1 1 ,STEP)) all-prototypes*)
			   
			   (reset-offsets! all-prototypes*)
			   (incf-view! all-prototypes* 1 (- id))))
		       bucket)
	     unless (= i (- D step))
	       do (progn
		    (incf-view! all-prototypes* 2 step)
		    (incf-view! a-offline* 1 step)))
       all-prototypes))))

;; B(t, i)
;; Each Bucket possess: tree-level, index, threshold, next-nodes
;; compare: subspace[index] > threshold

;; Todo Pprint
;; Todo: Unroll Macro: Bucket -> Node
(defstruct (Bucket
	    (:predicate bucket-)
	    (:constructor make-toplevel-bucket (indices &aux (tree-level 0) (id 0)))
	    (:constructor make-sub-bucket (indices tree-level id)))
  (tree-level tree-level :type fixnum)
  (i id :type fixnum)
  (scale  0.0 :type single-float)
  (offset 0.0 :type single-float)
  (threshold-quantized 0 :type fixnum)
  (index 0 :type fixnum) ;; split-index
  (threshold  0.0 :type single-float) ;; = split-val
  (threshold-candidates nil) ;; 0~dim
  (next-nodes nil :type list)
  (indices indices :type list)) ;; The list of indices of A which current bucket posses (C, D), D= 1 3 5 10 2 ... Bucketが管轄するDのIndex

(defun give-idx-to-buckets (buckets &key (nsplits 4))
  (with-bucket-clusters (id bucket nsplits buckets)
    (setf (bucket-i bucket) id)))
  

(defmethod col-variances ((bucket Bucket) subspace &aux (N (length (bucket-indices bucket))))
  (declare (optimize (speed 3))
	   (type matrix subspace))
  (with-view (s* subspace `(:indices ,@(bucket-indices bucket)) t)
    (with-caches ((mu `(,(car (shape s*)) 1) :place-key :col-variances1)
		  (ex  (shape s*) :place-key :col-variances2)
		  (result `(1 ,(second (shape s*))) :place-key :sum-out1))
      (%fill result 0.0) ;; %sum is just broadcasting
      (%fill mu     0.0)
      
      (%sum s* :axis 1 :out mu)                    ;;
      (%scalar-div mu (second (shape s*))) ;; mu <- mean(s*, axis=1)

      (with-view (mu mu t `(:broadcast ,(second (shape s*))))
	(%move s* ex) ;; ex <- s*
	(%subs ex mu) ;;
	(%square ex)  ;; (xi - mu)^2
	(%scalar-div ex N)
	(%sum ex :out result :axis 0)
	result))))

(defun col-means (bucket subspace out &aux (indices (bucket-indices bucket)))
  (declare (optimize (speed 3))
	   (type matrix subspace))
  (%scalar-div (%sum
		(view subspace `(:indices ,@indices))
		:out out
		:axis 0)
	       (max 1 (length indices))))

(defun optimize-split-thresholds! (bucket d dth tree-level subspace)
  "Pick up index-th threshold-candiates, and use it as bucket's threshold."
  (declare (optimize (speed 3))
	   (type bucket bucket)
	   (type fixnum d dth tree-level))

  (when (= (bucket-tree-level bucket) tree-level)
    (setf (bucket-index bucket) dth)
    (setf (bucket-threshold bucket) (nth d (reverse (bucket-threshold-candidates bucket))))
    (learn-quantized-params! bucket subspace dth))

  (let ((buckets (bucket-next-nodes bucket)))
    (when buckets
      (optimize-split-thresholds! (car buckets) d dth tree-level subspace)
      (optimize-split-thresholds! (cdr buckets) d dth tree-level subspace)))
  nil)

(defun optimize-bucket-splits! (bucket
				best-dim
				subspace
				&aux
				  (left-idx (+ (* 2 (bucket-i bucket)) 1)) ;; 2x + 1 
				  (right-idx (* (bucket-i bucket) 2))) ;; 2x
  "Splits bucket's binary-tree
split-val dim"
  (declare (optimize (speed 3))
	   (type bucket bucket)
	   (type matrix subspace)
	   (type fixnum best-dim right-idx left-idx))

  ;; if bucket-nodes = nil -> Create new
  ;; if t  -> Optimize the old one
  ;; Add: if indices = nil?

  (flet ((create-new-bucket (points id)
	   (make-sub-bucket points (1+ (bucket-tree-level bucket)) id))
	 (make-tflist-indices (tflist)
	   "(:tflist 1.0 0.0 1.0 ...) => (:indices 1 3 ...)"
	   (declare (type matrix tflist))
	   
	   ;; Assertion: tflist isn't view-matrix
	   (assert (not (cl-xmatrix::matrix-projected-p tflist)) nil "make-tflist-indices: Assertion Failed because the given tflist is a view-object.")

	   ;; To Add: matrix but dtype=bit.
	   (loop for i fixnum upfrom 0 below (first (shape tflist))
		 if (= (the single-float (1d-mat-aref tflist i)) 1.0)
		   collect i)))

    (let* ((jurisdictions (bucket-indices bucket))
	   (x             (view subspace `(:indices ,@jurisdictions) best-dim))
	   (split-val     (bucket-threshold bucket))
	   (left-side-points)
	   (right-side-points))

      (with-caches ((mask     (shape x) :place-key :mask1)
		    (not-mask (shape x) :place-key :mask-not1))

	;; FIXME: conversation between lisp-array and matrix...

	;; Note: Having avoided using maddness-hash but using cons to express binary-tree-structure, I am wondering this semantics below is currect?

	;; x_ij >  val  -> assign to right
	;; x_ij <= val  -> assign to left

	;; left-side node can be obtained by:
	;; (car nodes)

	;; right-side node can be obtained by:
	;; (cdr nodes)
	
	(%>  x split-val :out mask)      ;; left
	(%<= x split-val :out not-mask)  ;; right

	(setq left-side-points  (make-tflist-indices mask))
	(setq right-side-points (make-tflist-indices not-mask))
	
	;; When left side child is supposed to be nil...?
	;; either is filled with copy of bucket
	(when (= (the single-float (%sumup mask)) 0.0)
	  (setq left-side-points (bucket-indices bucket)))

	;; When right side child is supposed to be nil...?
	(when (= (the single-float (%sumup not-mask)) 0.0)
	  (setq right-side-points (bucket-indices bucket)))
	
	(if (null (bucket-next-nodes bucket))
	    ;; If bucket is the end of node...
	    ;; => Creates a new bucket-tree.
	    (progn
	      (setf (bucket-next-nodes bucket)
		    (cons (create-new-bucket left-side-points right-idx)
			  (create-new-bucket right-side-points left-idx)))
	      nil)
	    ;; Otherwise -> Go deeper and update nodes.
	    (let ((nodes (bucket-next-nodes bucket)))
	      ;; Update Current Bucket -> Go Deeper

	      (setf (bucket-indices (car nodes)) right-side-points)
	      (setf (bucket-indices (cdr nodes)) left-side-points)

	      ;; Update Left-side
	      (optimize-bucket-splits!
	       (car nodes)
	       best-dim
	       subspace)

	      ;; Update Right-Side
	      (optimize-bucket-splits!
	       (cdr nodes)
	       best-dim
	       subspace)))))
    nil))

(defun learn-binary-tree-splits (subspace STEP &key (nsplits 4) (verbose t) &aux (N (first (shape subspace))))
  "
The function learn-binary-tree-splits computes maddness-hash given subspace X.

=========================================
subspace:
   C      C
   ++     ++
 N ++   N ++ ... P_n ... Nth Prototype
   ++     ++

subspace will be splited into Bucket:
  C       C
  ++    N ++ <- B(tree-level, i)
N ++ ->
  ++    N ++ <- B(tree-level, i)

split-dim -> C
=========================================

4.1 Hash Function Family g(a)
 - MaddnessHash (BinaryTree)
 - 4.2 Learning the Hash-Function Parameters
 Let be B(t, i) the bucket which is helper structure where t is the tree's depth and is in the index in the node:
- Split Functions
- Loss: L(j, B) -> SSE

Figure:
              B(1, 1)                  | nth=0
         /----------------\            |
     B(2, 1)            B(2,2)         | nth=1
   /---------\        /---------\      |
B(3, 1)  B(3, 2)   B(3, 3)  B(3, 4)    | nth=2
                                       | ...
                                       | nth=nsplits
Inputs:
 - subspace Matrix[N, STEP]
 - C, D     Fixnum
 - nsplits The number of training, the original paper has it that setting 4 is always the best.

Thresholds - scalar, K-1
Split-Indices - 

X = [C, (0, 1, 2, ... D)]
"
  (declare (optimize (speed 3))
	   (type matrix subspace)
	   (type fixnum STEP nsplits)
	   (type boolean verbose))

  (let ((buckets (make-toplevel-bucket
		  ;; B(1, 1) possess all the elements in the subspace.
		  (loop for i fixnum upfrom 0 below N
			collect i))))
    (with-cache (col-losses `(1 ,STEP) :dtype (matrix-dtype subspace) :place-key :losses1)
      (%fill col-losses 0.0)
      ;; Utils
      (macrolet ((maybe-print (object &rest control-objects)
		   `(when verbose (format t ,object ,@control-objects))))
	
	;; Training
	(dotimes (nth-split nsplits)
	  (maybe-print "== (~a/~a) Training Binary Tree Splits =========~%" (1+ nth-split) nsplits)

	  ;; heuristic = bucket_sse
	  (%fill col-losses 0.0)
	  (sumup-col-sum-sqs! col-losses buckets subspace)

	  (with-facet (col-losses* col-losses :direction :simple-array)
	    ;; Sort By [Largest-Loss-Axis, ... , Smallest-Loss-Axis]
	    
	    (let* ((dim-orders (argsort col-losses* :test #'>))
		   (dim-size   (length dim-orders)))

	      (with-cache (total-losses `(1 ,dim-size) :place-key :total-loss)
		(%fill total-losses 0.0)
		;; Here, we tests all dims to obtain the best trying dim.
		;; depth = 0, 1, 2, ..., nth-split

		(loop for d fixnum upfrom 0
		      for dth in dim-orders
		      do (loop named training-per-bucket
			       for level fixnum from 0 to nth-split
			       do (when (optimal-val-splits! subspace buckets total-losses d dth level)
				    
				    (return-from training-per-bucket))))

		;; total-losses = `(Loss1 Loss2 Loss3 ... LossN) where N=axis.
		;; (The next time nsplits training, The axis whose Loss is large is computes ahaed of time. <- considering col-losses)
		;;

		(let* ((best-trying-dim (first (argsort (convert-into-lisp-array total-losses) :test #'<)))
		       ;; Transcript dim -> sorted dim
		       (best-dim (nth best-trying-dim dim-orders)))
		  (declare (type fixnum nth-split))

		  ;; apply this split to get next round of buckets
		  (optimize-split-thresholds! buckets best-trying-dim best-dim nth-split subspace)
		  
		  (optimize-bucket-splits!    buckets best-dim subspace))))))
	(when verbose
	  (maybe-print "Loss: ~a~%" (compute-bucket-loss buckets subspace)))
	buckets))))

(defun bucket-collect-thresholds (bucket tree-level dim)
  (let ((result))
    (with-bucket-clusters (idx buck tree-level bucket)
      (push (nth dim (bucket-threshold-candidates buck)) result))
    (reverse result)))

(defun maxmin (matrix dim)
  (let* ((sorts (sort-rows-based-on-col matrix dim))
	 (min-loss (%sumup (view matrix (car sorts) dim)))
	 (max-loss (%sumup (view matrix (car (last sorts)) dim))))
    (values min-loss max-loss)))

(defun maxmin-all (matrix)
  (with-facet (m* matrix :direction :simple-array)
    (let ((sorts (argsort m* :test #'>)))
      (values (coerce (aref m* (car sorts)) 'single-float)
	      (coerce (aref m* (car (last sorts))) 'single-float)))))

(defun learn-quantized-params! (bucket subspace best-dim)
  "Appendix B"
  (declare ;;(optimize (speed 3))
	   (type matrix subspace)
	   (type fixnum best-dim))
  (multiple-value-bind (min-loss max-loss) (maxmin subspace best-dim)
    (let* ((sorts (sort (copy-list (bucket-threshold-candidates bucket)) #'<))
	   (min-val (car sorts))
	   (max-val (car (last sorts)))
	   (offset (/ (+ min-loss min-val) 2))
	   (upper-val (- (/ (+ max-loss max-val) 2) offset))
	   (l (log (/ 254.0 upper-val) 2))
	   (scale (expt 2 l))
	   (quantized-threshold (round (* (- (bucket-threshold bucket) offset) scale))))
      (setf (bucket-scale bucket) scale)
      (setf (bucket-offset bucket) offset)
      ;; y = af(x)+b
      ;;(print (bucket-threshold bucket))
      ;;(print quantized-threshold)
      (setf (bucket-threshold-quantized bucket) quantized-threshold)
      nil)))

(declaim (ftype (function ((simple-array t (*)) &key (:test function)) list) argsort))
(defun argsort (array &key (test #'>))
  (declare (optimize (speed 3) (safety 0))
	   (type function test)
	   (type (simple-array t (*)) array))
  ;; Could be slow... Rewrite with C
  (mapcar #'second
          (stable-sort
           (loop
             for index fixnum from 0
             for element-i fixnum upfrom 0 below (array-total-size array)
             collect (list (aref array element-i) index))
	   test
           :key #'first)))

(defun sort-rows-based-on-col (matrix dim)
  "Returns a sorted indices based n matrix's cols."
  (declare (optimize (speed 3) (safety 0))
	   (type matrix matrix))
  (with-facet (arr* (view matrix t dim) :direction :simple-array)
    ;; (Smallest-Index ... Largetst-Index)
    (argsort arr* :test #'<)))


(defun optimal-val-splits! (subspace bucket total-losses d dim tree-level)
  "The function optimal-val-splits! explores the bucket's nodes untill reaches tree-level, and update total-losses.

Input:
   d   - whichth axis of the total-losses to set the result.
   dim - the axis to be used.

Return:
   - early-stopping-p
"
  (declare (optimize (speed 3) (safety 0))
	   (type bucket bucket)
	   (type matrix subspace total-losses)
	   (type fixnum d dim tree-level))
  (if (= (bucket-tree-level bucket) tree-level)
      (multiple-value-bind (split-val loss) (compute-optimal-val-splits subspace bucket dim)
	(declare (type single-float split-val loss))

	(with-view (loss-d total-losses t 0)
	  (incf-offsets! loss-d 0 d)
	  (%scalar-add loss-d loss)
	  (let ((loss-d* (%sumup loss-d)))
	    (declare (type single-float loss-d*))

	    ;; Note: split-val[dim~0] <- dont forget to rev it.
	    ;; that is, dim is on the around way.

	    ;; candidates:
	    ;; dim-order[2] ... dim-order[1] dim-order[0]
	    ;; obtained by d
	    (push split-val (bucket-threshold-candidates bucket))
	    ;; Judge early-stoppig-p
	    (if (= d 0)
		nil
		(%all?
		 (%satisfies
		  (view total-losses t `(0 ,d))
		  #'(lambda (x) (< (the single-float x) loss-d*))))))))
      (let ((next-nodes (bucket-next-nodes bucket)))
	;; Explore nodes until reach tree-level

	(when (null next-nodes)
	  (error "optimal-val-splits! Couldn't find any buckets."))

	(let ((res1 (optimal-val-splits! subspace (car next-nodes) total-losses d dim tree-level))
	      (res2 (optimal-val-splits! subspace (cdr next-nodes) total-losses d dim tree-level))
	      (res3 (optimal-val-splits! subspace bucket total-losses d dim (bucket-tree-level bucket)))) ;; compute current-level node.
	  (or res1 res2 res3)))))

(declaim (ftype (function (matrix Bucket fixnum) (values single-float single-float)) compute-optimal-val-splits))
(defun compute-optimal-val-splits (subspace bucket dim
				   &aux
				     (D (second (shape subspace))))
  "The function optimal-val-splits tests all possible thresholds to find one minimizing B(tree-level, i).


Ref: Appendix C, Algorithm 3, Optimal Split Threshold Within a Bucket.

subspace - original subspace
"
  (declare (optimize (speed 3))
	   (type matrix subspace)
	   (type Bucket bucket)
	   (type fixnum dim))

  ;; when the bucket is empty.
  (when (or (null (bucket-indices bucket))
	    (< (length (bucket-indices bucket)) 2))
    (return-from compute-optimal-val-splits (values 0.0 0.0)))

  (let* ((indices (bucket-indices bucket))
	 (x       (view subspace `(:indices ,@indices) t)) ;; Cut out matrices which given bucket indicates.
	 (x-sort-indices      (sort-rows-based-on-col x dim))
	 (x-sort-indices-rev  (reverse x-sort-indices))
	 (N (length x-sort-indices)))
    (declare (type list x-sort-indices))

    (with-caches ((x-head `(,N ,D) :dtype (matrix-dtype subspace) :place-key :C1)
		  (x-tail `(,N ,D) :dtype (matrix-dtype subspace) :place-key :C2)
		  (s-out  `(,N 1)  :dtype (matrix-dtype subspace) :place-key :C3))
      (%fill s-out 0.0)

      ;; If the SSE error for all rows is small, then the Bucket has similar rows classified.
      ;; Note: Replace Loss Functions into: Cosine Simirality.
      ;; Which excepted to be working as if Reformer.
      ;; The assumption is that a single (dim, val) set isn't enough to cluster a embedding vector.
      (cumulative-sse! (view x `(:indices ,@x-sort-indices))     x-head)
      (cumulative-sse! (view x `(:indices ,@x-sort-indices-rev)) x-tail)

      ;; x-head = sses-head, x-tail = sses-tail
      ;; losses <- sses-head 
      ;; losses[1:N-1] <- losses[1:N-1] + sses_tail[2:N]
      
      (%adds x-head x-tail)
      
      (%sum x-head :axis 1 :out s-out)

      ;; matrix->lisp-array conversations may contribute to low performance...
      ;; This could be reimplemented in C or define-vop.
      (with-facet (s-out* s-out :direction :simple-array)
	(let* ((best-idx (car (argsort s-out* :test #'<)))
	       (next-idx (min (the fixnum
				   (1- (the fixnum (car (shape subspace)))))
			      (the fixnum
				   (1+ best-idx))))
	       (col-idx1 (nth best-idx x-sort-indices))
	       (col-idx2 (nth next-idx x-sort-indices))
	       (c1 (view x col-idx1 dim))
	       (c2 (view x col-idx2 dim)))
	  (declare (type fixnum best-idx next-idx))
	  ;; c1 c2 = [1, 1]
	  ;; %sumup may slow... -> Add: mats-as-scalar
          ;; when dtype=uint?

	  ;; (values use-split-val use-loss)
	  
	  (values (/ (+ (the single-float (%sumup c1))
			(the single-float (%sumup c2)))
		     2.0)
		  (the single-float (%sumup (view s-out best-idx)))))))))


(defun cumulative-sse! (xp
			cumsses
			&aux
			  (N (car    (shape xp)))
			  (D (second (shape xp)))
			  (dtype     (dtype xp)))
  "Algorithm 4 Cumulative SSE. (Computes SSE Loss)

   Input: X [N D]
          out - the matrix to be overwritten with result. If nil, The function allocates a new matrix.
   Output: nil"
  (declare (optimize (speed 3))
	   (type index N D)
	   (type matrix xp cumsses))
  
  (with-caches ((cumX-cols  `(1 ,D) :dtype dtype :place-key  :cumsse-col1)
		(cumX2-cols `(1 ,D) :dtype dtype :place-key :cumsse-col2)
		(x          `(,N ,D) :dtype dtype :place-key :cognitious-x))
    (%fill cumX-cols 0.0)
    (%fill cumX2-cols 0.0)
    (%move xp x) ;; x-matrix's bug: move :indices-array into cognitious array.
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
    nil))

(defun compute-bucket-loss (bucket subspace &aux (N (length (bucket-indices bucket))))
  (let ((loss (%sumup (%scalar-mul (Col-variances bucket subspace) n))))
    (if (> loss 0.0)
	loss
	0.0)))

(defun sumup-col-sum-sqs! (place bucket subspace &aux (N (length (bucket-indices bucket))))
  (declare (optimize (speed 3))
	   (type matrix place subspace)
	   (type bucket bucket))
  (when (bucket-indices bucket)
    (%adds place (%scalar-mul (col-variances bucket subspace) n)))

  (let ((children (bucket-next-nodes bucket)))
    (when children
      (sumup-col-sum-sqs! place (car children) subspace)
      (sumup-col-sum-sqs! place (cdr children) subspace)))
  nil)

;; More TO DO
;; 8Bit Quantize
;; APPLY_HASH_FUNCTIOn
;; Prototype Optimizing
;; Construct LUT

#|
mithral_encode_fp32_t(const float *X,
			     int64_t nrows,
			     int ncols,
			     const uint32_t *splitdims,
			     const int8_t *all_splitvals,
			     const float *shifts,
			     const float *offsets,
			     int ncodebooks,
uint8_t *out)

  void mithral_scan_fp32_t(const uint8_t* encoded_mat,
			   int ncodebooks,
			   int M,
			   const uint8_t* luts,
uint8_t* out_mat) {

  void mithral_lut_fp32_t(const float *Q, int nrows, int ncols, int ncodebooks,
                       const float *centroids, float &out_offset_sum,
                       float &out_scale, float *__restrict__ tmp_lut_f32,
			  uint8_t *out) {
|#

(defcfun ("mithral_encode_fp32_t" maddness-encode-c) :void
  (X-pointer  (:pointer :float))
  (nrows       :int64)
  (ncols       :int)
  (splitdims   (:pointer :uint32))
  (all-splitvals (:pointer :uint8))
  (shifts      (:pointer :float))
  (offsets     (:pointer :float))
  (ncodebooks  :int) ;; nsplits
  (out-pointer (:pointer :uint8)))

(defcfun "mithral_scan_fp32_t" :void
  (encoded-mat (:pointer :uint8))
  (ncodebooks :int)
  (m :int)
  (luts (:pointer :uint8))
  (out  (:pointer :uint8)))

(defcfun "mithral_lut_fp32_t" :void
  (B (:pointer :float))
  (nrows :int)
  (ncols :int)
  (K     :int)
  (protos (:pointer :float))
  (out-offset-sum (:pointer :float))
  (out-scale      (:pointer :float))
  (tmp-lut-f32    (:pointer :float)) ;; tmp-lut-f32.shape == out.shape
  (out            (:pointer :uint8)))


(defun maddness-create-and-quantize-luts (protos B C K &aux (M (car (shape B))))
  "LUT -> MCK"
  (with-caches ((out-lut-f32 `(,M ,C ,K) :dtype :float :place-key :lutf32)
		(out-lut     `(,M ,C ,K) :dtype :uint8 :place-key :lutuint8))
    (let ((out-offset-sum* (foreign-alloc :float :initial-element 0.0))
	  (out-scale*      (foreign-alloc :float :initial-element 0.0)))
      (mithral-lut-fp32-t
       (matrix-vec B)
       (car    (shape B))
       (second (shape B))
       K
       (matrix-vec protos)
       out-offset-sum*
       out-scale*
       (matrix-vec out-lut-f32)
       (matrix-vec out-lut))
      (values out-lut
	      (mem-aref out-scale*      :float)
	      (mem-aref out-offset-sum* :float)))))

;; mithral_dense_lut_f32
;; centroids = protos
;; Q* = B



(defun maddness-lut! (out-lut b protos)
  ;; out-lut [B[0] ,C ,K]
  ;; B [M D]
  (let ((b-flatten (cl-xmatrix::reshape b `(1 1 ,(apply #'* (shape b))))))
    (with-caches ((out (shape protos) :dtype :float :place-key :ftmp)
		  (out-tmp `(,(car (shape out-lut)) ,(second (shape out-lut)) 1) :place-key :sum-out-tmp))
      (with-view (b-flatten b-flatten `(:broadcast ,(car (shape out))) `(:broadcast ,(second (shape out))) t)
	(%move b-flatten out)
	(%subs out protos)
	(%sum out :axis 2 :out out-tmp)
	(%move out-tmp out-lut)))))

(defun max-multiple-axis=0and2 (matrix C)
  ;; min(i, j) = matrix where matrix = (1 C)
  (with-cache (out `(1 ,C) :dtype :float :place-key :max2)
    (dotimes (i C)
      ;; explore matrix[0~X, :, 0~Z]
      (multiple-value-bind (max min) (maxmin-all (view matrix t i t))
	(declare (ignore min))
	(setf (1d-mat-aref out i) max)))
    out))

(defun min-multiple-axis=0and2 (matrix C)
  ;; min(i, j) = matrix where matrix = (1 C)
  (with-cache (out `(1 ,C) :dtype :float :place-key :min2)
    (dotimes (i C)
      ;; explore matrix[0~X, :, 0~Z]
      (multiple-value-bind (max min) (maxmin-all (view matrix t i t))
	(declare (ignore max))

	(setf (1d-mat-aref out i) min)))
    out))

(defun argmax (matrix)
  (with-facet (m* matrix :direction :simple-array)
    (let ((sorts (argsort m* :test #'>)))
      (aref m* (car sorts)))))

(defun maddness-quantize-luts! (lut)
  "Appendix A: Quantizing Look up Tables"
  ;;lut
  ;; tmp define for scalar
  (let ((max (max-multiple-axis=0and2 lut (second (shape lut))))
	(min (min-multiple-axis=0and2 lut (second (shape lut)))))
    (let* ((gaps (%subs max min))
	   (gap (argmax gaps))
	   (exponent (+ 1 (round (if (= gap 0.0)
				3.40e5
				(log gap 2)))))
	   (scale (expt 2 (- exponent)))
	   (scale (* scale (- 255.5 1e-10)))
	   (offsets (view (cl-xmatrix::reshape min `(1 ,(second (shape lut)) 1)) `(:broadcast ,(car (shape lut))) t `(:broadcast ,(third (shape lut))))))
      (with-caches ((lut-tmp       (shape lut) :dtype :float :place-key :ltmp)
		    (lut-quantized (shape lut) :dtype :uint8 :place-key :lut-q))
	(%move lut lut-tmp)
	(%subs lut-tmp offsets)
	(%scalar-mul lut-tmp scale)
	(%scalar-add lut-tmp 0.5)

	(%index lut-quantized
		#'(lambda (i) (print (1d-mat-aref lut i)) (round (print (1d-mat-aref lut-tmp i)))))
	(values lut-quantized scale (%sumup offsets))))))

;; not used
(defun create-luts (protos B C K)
  (declare (type matrix protos b)
	   (type fixnum C K))
  ;; einsum(CKd, McD -> MCK)
  ;; M C K
  (with-cache (lut `(,(car (shape B)) ,C ,K) :place-key :lut-cache)
    (loop for i fixnum upfrom 0 below (car (shape B))
	  do (with-views ((lut* lut t t i)
			  (b* B i))
	       
	       (maddness-lut! lut* b* protos)))
    (maddness-quantize-luts! lut)))

(defun flatten (lst)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (listp el)
                   (setf acc (rflatten el acc))
                   (push el acc)))
             acc))
    (reverse (rflatten lst nil))))

(defun flatten-bucket (bucket slot)
  (declare (optimize (speed 3))
	   (type bucket bucket))
  (let ((result))
    (labels ((explore (bucket)
	       (with-slots ((children next-nodes))
		   bucket
		 (push (slot-value bucket slot) result)
		 (when children
		   (explore (cdr children))
		   (explore (car children))))))
      (explore bucket)
      (reverse result))))

(defun flatten-buckets1 (buckets)
  "[Proto_1(Bucket_0), Proto_2(Bucket_0), ...Proto_0(Bucket_1), ...]"
  (declare (type list buckets))
  (labels ((collect (name dtype)
	     (let ((result (flatten (map 'list #'(lambda (b) (flatten-bucket b name)) buckets))))
	       (from-facet
		`(1 ,(length result))
		result
		:direction :list
		:dtype dtype))))
    
    (values
     (collect 'scale :float)
     (collect 'offset :float)
     (collect 'threshold-quantized :int)
     (collect 'index :int))))

(defun gather-bucket (bucket slot tree-level)
  (declare (optimize (speed 3))
	   (type bucket bucket)
	   (type fixnum tree-level))
  (let ((result))
    (labels ((explore (bucket)
	       (with-slots ((children next-nodes) (tlevel tree-level))
		   bucket
		 (declare (type fixnum tlevel))
		 (when (= tree-level tlevel)
		   (push (slot-value bucket slot) result))
		 (when children
		   (explore (cdr children))
		   (explore (car children))))))
      (explore bucket)
      (reverse result))))

(defun flatten-buckets (buckets &key (nsplits 4))
  (labels ((collect (name)
	     (loop for i fixnum upfrom 0 below nsplits
		   nconc
		   (loop for b in buckets
			 nconc (gather-bucket b name i))))
	   (findsall (name dtype)
	     (let ((result (collect name)))
	       (from-facet
		`(1 ,(length result))
		result
		:direction :list
		:dtype dtype))))
    (values
     (findsall 'scale :float)
     (findsall 'offset :float)
     (findsall 'threshold-quantized :int)
     (findsall 'index :int))))

(defun %lognot (matrix)
  (%scalar-mul matrix -1.0)
  (%scalar-add matrix 1.0)
  matrix)

(defun tflist->indices (tflist &key (lognot nil))
  "(:tflist 1.0 0.0 1.0 ...) => (:indices 1 3 ...)"
  (declare (type matrix tflist))
  
  ;; Assertion: tflist isn't view-matrix
  (assert (not (cl-xmatrix::matrix-projected-p tflist)) nil "make-tflist-indices: Assertion Failed because the given tflist is a view-object.")

  ;; To Add: matrix but dtype=bit.
  (loop for i fixnum upfrom 0 below (first (shape tflist))
	if (if lognot
	       (not (= (the single-float (1d-mat-aref tflist i)) 0.0))
	       (= (the single-float (1d-mat-aref tflist i)) 1.0))
	  collect i))

;; Not anymore used.
(defun apply-hash-function! (A idx-out idx-tmp bucket)
  (declare (optimize (speed 3))
	   (type matrix A idx-out idx-tmp)
	   (type Bucket bucket))

  ;; Move: X-Axis
  ;; A[N, 1]は多分SIMD化されない・・・
  ;; Original実装との相違点
  ;; 速度の利益のTradeoff：
  ;; 扱う行列のサイズが小さいこと << 毎回の扱う行列が固定である
  ;; IDXをN=0からN=1に対して書き込んでいく MaddnessHashに従う
  ;; idx*を一次領域に使う
  ;; A ... [N, 1]
  ;; colum-major order is needed...?
  (with-slots ((v threshold) (children next-nodes)) bucket
    (declare (type single-float v))

    ;; apply for n times.

    ;; (car children) => left
    ;; (cdr children) => Right
    
    ;; A_ij >  v -> Assign to right: next_idx = 2i+1
    ;; A_ij <= v -> Assign to left : next_idx = 2i

    ;; MaddnessHash
    (with-unsafe ;; Shapeの形状を確認しない
      (%fill idx-tmp 0.0)
      (%>= A v :out idx-tmp) ;; FIXME: THE RESULT IS SINGLE_FLOAT...
      (%scalar-mul idx-out 2.0)
      (%adds idx-out idx-tmp))

    (when children
      ;; :tflistを使うとSIMD化されない
      (setq idx-tmp (%copy idx-tmp))
      
      (let ((left-indices  (tflist->indices idx-tmp))
	    (right-indices (tflist->indices idx-tmp :lognot t)))
	(declare (type list left-indices right-indices))
	;; Go left
	(unless (= (length left-indices) 0)
	  (apply-hash-function!
	   (view a       `(:indices ,@left-indices))
	   (view idx-out `(:indices ,@left-indices))
	   idx-tmp
	   (car children)))
	
	;; Go right
	(unless (= (length right-indices) 0)
	  (apply-hash-function!
	   (view a       `(:indices ,@right-indices))
	   (view idx-out `(:indices ,@right-indices))
	   idx-tmp
	   (cdr children))))
      nil)))

;; Not Anymore used.
(defun maddness-encode (buckets
			prototypes
			A
			C
			&aux
			  (N    (first  (shape A)))
			  (D    (second (shape A)))
			  (STEP (/ D C)))
  "Buckets ... each subspace's bucket
prototypes ... prototypes obtained by training process.
A[N D] ... matrix to be encoded.

+++++
+++++
+++++"
  (declare (optimize (speed 3)) ;; safety 0
	   (type fixnum C N D STEP)
	   (type list buckets)
	   (type matrix A))

  ;; A[N, D] => A[N, C]
  ;; Prototype:[STEP, 0~4] -> 1,2,3,4,...16のIndexを振り分ける
  ;; FIXME: FLOAT _> INT
  (with-caches ((idxs    `(,N ,C) :place-key :encode-cache :dtype :float)
		(idx-tmp `(,N 1) :place-key  :idxs-tmp     :dtype :float))
    (%fill idxs 0.0)
    (%fill idx-tmp 0.0)
    ;; x's each proto -> idxs.
    (with-views ((idxs* idxs t 0)
		 (A*    A    t 0))
      ;; Move: Y axis
      (loop for i fixnum upfrom 0 below D by STEP
	    for c fixnum upfrom 0
	    do (let ((best-dim (bucket-index (nth c buckets))))
		 (incf-view! A* 1 best-dim)
		 (apply-hash-function! A* idxs* idx-tmp (nth c buckets))
		 (incf-view! A* 1 (- best-dim)))
	    unless (= i (- D step))
	      do (progn
		   (incf-view! A*    1 STEP)
		   (incf-view! idxs* 1 1))))
    (print idxs)))

#|
90% of computation time is alloc-mat.
  seconds  |     gc     |     consed    |    calls   |  sec/call  |  name  
----------------------------------------------------------------
     4.562 |      0.011 | 1,071,901,488 |      5,243 |   0.000870 | CL-XMATRIX::ALLOCATE-MAT
     0.486 |      0.000 |    19,214,592 |     14,836 |   0.000033 | CL-XMATRIX::COMPUTE-ABSOLUTE-SUBSCRIPT
     0.136 |      0.000 |             0 |  2,314,320 |   0.000000 | CL-XMATRIX::FP32-SCALAR-MUL
     0.123 |      0.000 |             0 |  2,405,500 |   0.000000 | CL-XMATRIX::FP32-COPY
     0.103 |      0.002 |    74,127,520 |  2,315,526 |   0.000000 | INCF-OFFSETS!
     0.085 |      0.000 |             0 |  3,473,934 |   0.000000 | CL-XMATRIX::FP32-ADD
     0.055 |      0.000 |             0 |  1,159,710 |   0.000000 | CL-XMATRIX::FP32-MUL
     0.039 |      0.000 |             0 |  2,323,661 |   0.000000 | DTYPE->LISP-TYPE
     0.029 |      0.000 |     3,965,424 |     22,045 |   0.000001 | CL-XMATRIX::PARSE-SUBSCRIPTS
     0.020 |      0.000 |     4,941,904 |     97,084 |   0.000000 | 1D-MAT-AREF
     0.019 |      0.000 |    28,350,464 |     44,090 |   0.000000 | CL-XMATRIX::PARSE-RELATIVE-POSITION
     0.011 |      0.000 |             0 |     11,343 |   0.000001 | CL-XMATRIX::READ-THREAD-CACHED-OBJECT
     0.009 |      0.000 |             0 |      4,044 |   0.000002 | FREE-MAT
     0.005 |      0.000 |             0 |     58,890 |   0.000000 | CL-XMATRIX::VIEW-ENDINDEX
     0.004 |      0.000 |     8,691,232 |      2,438 |   0.000002 | CONVERT-INTO-LISP-ARRAY
     0.004 |      0.000 |       455,168 |     16,099 |   0.000000 | CL-XMATRIX::GET-STRIDE
     0.004 |      0.000 |             0 |      4,044 |   0.000001 | CL-XMATRIX::SET-THREAD-CACHED-OBJECT
     0.003 |      0.000 |             0 |      7,639 |   0.000000 | COERCE-TO-DTYPE
     0.003 |      0.000 |             0 |     10,486 |   0.000000 | DTYPE-P
     0.003 |      0.000 |       617,776 |     10,620 |   0.000000 | (SETF 1D-MAT-AREF)
     0.002 |      0.000 |             0 |      6,147 |   0.000000 | CL-XMATRIX::FP32-FILL
     0.002 |      0.000 |     3,560,560 |     22,045 |   0.000000 | CL-XMATRIX::VIEW-OF-MATRIX-WITH-SHAPE
     0.002 |      0.000 |       260,096 |      8,049 |   0.000000 | CL-XMATRIX::VISIBLE-SHAPE
     0.001 |      0.000 |             0 |        192 |   0.000007 | CL-XMATRIX::FP32-SCALAR-DIV
     0.001 |      0.000 |       422,656 |     16,099 |   0.000000 | CL-XMATRIX::FILL-WITH-D
     0.001 |      0.000 |             0 |     43,232 |   0.000000 | CL-XMATRIX::FP32-SCALAR-GREATER-THAN
     0.001 |      0.000 |        32,768 |      6,317 |   0.000000 | %SUMUP
     0.001 |      0.000 |             0 |     58,890 |   0.000000 | CL-XMATRIX::VIEW-STARTINDEX
     0.001 |      0.000 |             0 |         96 |   0.000005 | CL-XMATRIX::FP32-SUB
     0.000 |      0.000 |             0 |      4,812 |   0.000000 | RESET-OFFSETS!
     0.000 |      0.000 |        32,512 |      8,049 |   0.000000 | CL-XMATRIX::COMPUTE-VISIBLE-AND-BROADCASTED-SHAPE
     0.000 |      0.000 |             0 |      1,302 |   0.000000 | CL-XMATRIX::FP32-SCALAR-ADD
     0.000 |      0.000 |             0 |     44,090 |   0.000000 | CL-XMATRIX::PARSE-BROADCAST
     0.000 |      0.000 |             0 |      3,605 |   0.000000 | DTYPE
     0.000 |      0.000 |             0 |      4,044 |   0.000000 | CL-XMATRIX::MATRIX-FREEP
     0.000 |      0.000 |             0 |      4,044 |   0.000000 | (SETF CL-XMATRIX::MATRIX-FREEP)
     0.000 |      0.000 |             0 |      2,396 |   0.000000 | MATRIX-DTYPE
     0.000 |      0.000 |             0 |      4,044 |   0.000000 | CL-XMATRIX::MATRIX-VEC
     0.000 |      0.000 |             0 |          3 |   0.000001 | INCF-VIEW!
     0.000 |      0.000 |        65,024 |      2,806 |   0.000000 | CL-XMATRIX::CALL-WITH-VISIBLE-AREA-AND-EXTOPE
     0.000 |      0.000 |             0 |  1,330,040 |   0.000000 | CL-XMATRIX::SYSTEM-SET-VIEW!
     0.000 |      0.000 |        65,024 |      8,049 |   0.000000 | CL-XMATRIX::CALC-STRIDES
     0.000 |      0.000 |       261,616 |      2,806 |   0.000000 | CL-XMATRIX::VIEW-OF-MATRIX
     0.000 |      0.000 |             0 |     43,232 |   0.000000 | CL-XMATRIX::FP32-SCALAR-LESS-THAN-EQ
     0.000 |      0.000 |       652,864 |      5,243 |   0.000000 | MATRIX
     0.000 |      0.000 |        32,736 |        104 |   0.000000 | %<=
     0.000 |      0.000 |             0 |        192 |   0.000000 | %SCALAR-DIV
     0.000 |      0.000 |    76,014,096 |  2,314,320 |   0.000000 | %SCALAR-MUL
     0.000 |      0.000 |        65,536 |      1,198 |   0.000000 | %SATISFIES
     0.000 |      0.000 |             0 |      1,198 |   0.000000 | %ALL?
     0.000 |      0.000 |    63,184,912 |  9,460,424 |   0.000000 | CALL-WITH-VISIBLE-AREA
     0.000 |      0.000 |       131,072 |      6,147 |   0.000000 | %FILL
     0.000 |      0.000 |             0 |        104 |   0.000000 | %>
     0.000 |      0.000 |       682,752 |     22,045 |   0.000000 | VIEW
     0.000 |      0.000 |        32,752 |      1,302 |   0.000000 | %SCALAR-ADD
     0.000 |      0.000 |    38,759,632 |  1,159,710 |   0.000000 | %MULS
     0.000 |      0.002 |   107,188,016 |  3,473,934 |   0.000000 | %ADDS
     0.000 |      0.002 |    38,661,568 |  1,164,522 |   0.000000 | %MOVE
     0.000 |      0.000 |             0 |      2,396 |   0.000000 | COERCE-TO-MAT-DTYPE
     0.000 |      0.000 |             0 |      2,598 |   0.000000 | %SQUARE
     0.000 |      0.000 |       130,032 |      1,299 |   0.000000 | %SUM
     0.000 |      0.000 |        32,752 |         96 |   0.000000 | %SUBS
----------------------------------------------------------------
     5.720 |      0.017 | 1,542,534,544 | 33,542,773 |            | Total

  seconds  |     gc     |     consed    | calls |  sec/call  |  name  
-----------------------------------------------------------
     4.215 |      0.013 |   808,529,824 | 1,330 |   0.003169 | COMPUTE-OPTIMAL-VAL-SPLITS
     2.758 |      0.003 |   692,267,984 | 2,372 |   0.001163 | CUMULATIVE-SSE!
     0.213 |      0.000 |    44,408,416 |    92 |   0.002319 | COL-VARIANCES
     0.169 |      0.000 |    33,799,472 | 1,186 |   0.000142 | SORT-ROWS-BASED-ON-COL
     0.165 |      0.002 |    75,712,528 | 2,404 |   0.000068 | ARGSORT
     0.065 |      0.000 |    12,810,576 |    16 |   0.004050 | OPTIMIZE-BUCKET-SPLITS!
     0.019 |      0.000 |     3,875,584 |   549 |   0.000034 | OPTIMAL-VAL-SPLITS!
     0.001 |      0.000 |             0 |    16 |   0.000055 | SUMUP-COL-SUM-SQS!
     0.000 |      0.000 |             0 |    16 |   0.000007 | OPTIMIZE-SPLIT-THRESHOLDS!
     0.000 |      0.000 |             0 |   120 |   0.000001 | MAKE-SUB-BUCKET
     0.000 |      0.000 |             0 |     4 |   0.000001 | MAKE-TOPLEVEL-BUCKET
     0.000 |      0.000 |       195,072 |     1 |   0.000000 | INIT-AND-LEARN-OFFLINE
     0.000 |      0.000 |        32,512 |     4 |   0.000000 | LEARN-BINARY-TREE-SPLITS
-----------------------------------------------------------
     7.604 |      0.018 | 1,671,631,968 | 8,110 |            | Total

|#


(defun print-bucket-with-subspace (bucket subspace &key (stream t) (indent 0))
  "Visualizes the bucket."
  (macrolet ((iformat (stream content &rest args)
	       `(progn
		  (dotimes (i (* 2 indent)) (princ " " ,stream))
		  (format ,stream ,content ,@args))))
    (format stream "~a"
	    (with-output-to-string (out)
	      (multiple-value-bind (_ loss) (compute-optimal-val-splits subspace bucket (bucket-index bucket))
		(declare (ignore _))
		(iformat out "B(t=~a, i=~a) <N=~a, dim=~a, threshold=~a> {SSE -> ~a} ~%" indent (bucket-i bucket) (length (bucket-indices bucket)) (bucket-index bucket) (bucket-threshold bucket) loss))
	      
	      (when (bucket-next-nodes bucket)
		(print-bucket-with-subspace
		 (car (bucket-next-nodes bucket))
		 subspace
		 :stream out
		 :indent (1+ indent))
		(print-bucket-with-subspace
		 (cdr (bucket-next-nodes bucket))
		 subspace
		 :stream out
		 :indent (1+ indent)))))))

;; N*D @ D*M
(defclass MaddnessMatmul ()
  ((N :initarg :N :type fixnum :reader mithral-n)
   (D :initarg :D :type fixnum :reader mithral-d)
   (M :initarg :M :type fixnum :reader mithral-m)
   (C :initarg :C :type fixnum :reader mithral-c)
   (nsplits :initarg :nsplits :type fixnum :reader mithral-nsplits)
   (K :type fixnum :reader mithral-k)
   (luts :type matrix   :writer write-luts)
   (protos :type matrix :writer write-protos)
   (buckets :type list :writer write-buckets)
   (A-enc :type matrix :writer write-a-enc)
   (B-mat :type matrix :writer write-b-mat)
   (alpha :type single-float :writer write-alpha)
   (beta  :type single-float :writer write-beta)
   (scales    :type matrix :writer write-scales :reader mithral-scales)
   (offsets   :type matrix :writer write-offsets :reader mithral-offsets)
   (splitdims :type matrix :writer write-splitdims :reader mithral-splitdims)
   (splitvals :type matrix :writer write-splitvals :reader mithral-splitvals)))

(defun make-mithral (N D M C nsplits)
  ;; To Add: Assertion
  (make-instance 'MaddnessMatmul
		 :N n
		 :D d
		 :M m
		 :C C
		 :nsplits nsplits))

(defmethod initialize-instance :after ((maddness MaddnessMatmul) &key &allow-other-keys)
  (with-slots ((K K) (nsplits nsplits)) maddness
    (setf K (expt 2 nsplits))))

(defmethod set-a-offline ((maddness MaddnessMatmul) a-offline)
  (multiple-value-bind (buckets protos) (learn-prototypes-and-hash-function a-offline (mithral-c maddness))
    (multiple-value-bind (scales offsets thresholds split-dim)
	(flatten-buckets buckets)
      
      (write-protos  protos maddness)
      (write-buckets buckets maddness)
      
      (write-scales    scales maddness)
      (write-offsets   offsets maddness)
      (write-splitdims split-dim maddness)
      (write-splitvals thresholds maddness))))

(defmethod set-a ((maddness MaddnessMatmul) A)
  ;; Encode A
  (declare (optimize (speed 3))
	   (type matrix A))
  (with-cache (out `(,(car (shape A)) ,(mithral-k maddness)) :place-key :out-cache :dtype :uint8)
    
    (maddness-encode-c
     (matrix-vec a)
     (car    (shape A)) ;; N
     (second (shape A)) ;; D
     (matrix-vec (mithral-splitdims maddness))
     (matrix-vec (mithral-splitvals maddness))
     (matrix-vec (mithral-scales maddness))
     (matrix-vec (mithral-offsets maddness))
     (mithral-k maddness)
     (matrix-vec out))
    (write-a-enc out maddness)
    nil))

(defmethod set-b ((maddness MaddnessMatmul) B)
  ;; Create_Luts from B
  #|
  (multiple-value-bind (luts alpha beta)
      (create-luts
       (slot-value maddness 'protos)
       B
       (mithral-c maddness)
       (mithral-k maddness))
    (write-luts luts maddness)
    (write-alpha alpha maddness)
    (write-beta beta maddness)
  (write-b-mat B maddness)
  |#
  (multiple-value-bind (luts alpha beta)
      (maddness-create-and-quantize-luts
       (slot-value maddness 'protos)
       B
       (mithral-c maddness)
       (mithral-k maddness))
    (write-luts luts maddness)
    (write-alpha alpha maddness)
    (write-beta beta maddness)
    (write-b-mat B maddness)
    nil))


(declaim (ftype (function (matrix single-float single-float) matrix) retain-fp32-matrix))
(defun retain-fp32-matrix (matrix alpha beta)
  "Computes y=ax+b"
  (declare (optimize (speed 3) (safety 0))
	   (type single-float alpha beta))
  
  (with-cache (m* (shape matrix) :dtype :float :place-key :m1)
    ;; Rewrite it in C cuz it slow
    (%index m* #'(lambda (i)
		   (+ (* alpha (the fixnum (1d-mat-aref matrix i))) beta)))
    m*))

(defmethod calc-matmul ((maddness MaddnessMatmul)
			&aux
			  (M (mithral-m maddness))
			  (N (mithral-n maddness)))
  ;; Scan
  (with-cache (out `(,N ,M) :place-key :matmul-result :dtype :uint8)
    (mithral-scan-fp32-t
     (matrix-vec (slot-value maddness 'a-enc))
     (mithral-k maddness)
     M
     (matrix-vec (slot-value maddness 'luts))
     (matrix-vec out))

    (retain-fp32-matrix out (slot-value maddness 'alpha) (slot-value maddness 'beta))))

(defmethod display-all-the-buckets ((maddness MaddnessMatmul) subspace)
  (dolist (i (slot-value maddness 'buckets))
    (print-bucket-with-subspace i subspace)))

(defmethod show-stats ((maddness maddnessMatmul))
  "A-luts -> T?"
  )

(defun move-array-to-matrix (xmat mgl-mat &aux (size (apply #'* (shape xmat))))
  (mgl-mat:with-facet (m* (mgl-mat 'mgl-mat:backing-array :direction :output))
    (loop for i fixnum upfrom 0 below size
	  do (setf (aref m* i) (1d-mat-aref xmat i)))))

;; Add: adjust! (for optimizing with-cache)
(defun test (&key
	       (alpha 5.0)
	       (beta 2.0)
	       (N 1280) (D 64) (M 32) (C 16) (nsplits 4) (try-n 1000))
  ;; (mod N 32) == 0
  ;; How tall matrix is, computation time is constant.
  (let ((matrix  (matrix `(,N ,D)))
	(matrix1 (matrix `(,M ,D))))
    
    (%index matrix  #'(lambda (i)
			(declare (ignorable i))
			(cl-xmatrix::beta-bb alpha alpha beta)))
    
    (%index matrix1 #'(lambda (i)
			(declare (ignorable i))
			(cl-xmatrix::beta-bb alpha alpha beta)))


    ;;(sb-ext:gc :full t)
    ;;(sb-profile:profile "CL-XMATRIX")
    (let ((maddness (make-mithral N D M C nsplits)))
      (time (set-a-offline maddness matrix))  ;; Offline Training

      (clear-caches)
      (sb-ext:gc :full t)
      
      (time (set-b maddness matrix1)) ;; Creating-Luts
      ;;(time (set-a maddness matrix)) ;; set matrix (including alloc)

      (clear-caches)
      (sb-ext:gc :full t)
      
      (time (set-a maddness matrix)) ;; set matrix
      
      ;; (display-all-the-buckets maddness (view matrix t `(0, (/ D C))))
      
      (let ((result (calc-matmul maddness)))
	(print "RESULT")
	(print result))

      ;; matrix @ matrix1
      (format t "~%try_n=~a~%" try-n)
      (format t "~%Benchmarking matmul on Maddness (SBCL and C++).~%Size: N*D @ (D*M).T where N=~a D=~a M=~a" N D M)
      (time (dotimes (i try-n)
	      (set-a maddness matrix)
	      (calc-matmul maddness)))

      (clear-caches)
      (sb-ext:gc :full t)
      
      (let ((ma (mgl-mat:make-mat `(,N ,D) :ctype :float))
	    (mb (mgl-mat:make-mat `(,M ,D) :ctype :float))
	    (mc (mgl-mat:make-mat `(,N ,M) :ctype :float)))

	(move-array-to-matrix matrix ma)
	(move-array-to-matrix matrix1 mb)
	
	(format t "~%Benchmarking matmul on OpenBLAS (mgl-mat).~%")
	(time (dotimes (i try-n)
		(mgl-mat:gemm! 1.0 ma mb 0.0 mc :transpose-b? t)))
	(print (mgl-mat:gemm! 1.0 ma mb 0.0 mc :transpose-b? t))

	(clear-caches)
	(sb-ext:gc :full t)

	))))
;;(sb-profile:report)
;;(sb-profile:unprofile "CL-XMATRIX")
