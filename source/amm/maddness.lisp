
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.maddness
  (:use :cl :cl-xmatrix)
  (:export
   ))

(in-package :cl-xmatrix.amm.maddness)

;; This is the my reimplementation of Maddness

;; Indicesの高速化
;; TODO: Comments for readers

(defun init-and-learn-offline (a-offline
			       C
			       &key
				 (all-prototypes-out nil)
			       &aux
				 (N (car (shape a-offline)))
				 (D (second (shape a-offline)))
				 (K 16))
  "
The function init-and-learn-offline clusters the prototypes, and constructs the encoding function g(a).

Assertions:
  1. D must be divided by C (while the original impl doesn't impose it.)
  2. a-offline is a 2d-matrix.

Semantics:
=========================================================================
   D        C 
  +++       +--
N +++ ->  N +-- <- N*D Matrix is disjointed into N*C Matrix.
  +++       +--    Binary-Tree-Split is applied into each visible area.
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

  (assert (= (mod D C) 0) nil "Assertion Failed with (= (mod N C) 0). N=~a C=~a" N C)

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

  (let ((all-prototypes (or all-prototypes-out
			    (matrix `(,C ,K ,D) :dtype (dtype a-offline))
			    ))
	(step (/ D C)))
    (declare (ignore all-prototypes))
    
    (with-view (a-offline* a-offline t `(0 ,step))
      ;; subspace = [N, STEP]
      (loop for i fixnum upfrom 0 below D by step
	    do (progn
		 (learn-binary-tree-splits a-offline* STEP)
		 ;;(print i)
		 ;;(print a-offline*)
		 )
	    unless (= i (- D step))
	      do (incf-view! a-offline* 1 step))

      )))

;; B(t, i)
;; Each Bucket possess: tree-level, index, threshold, next-nodes
;; compare: subspace[index] > threshold

;; Todo Pprint
;; Todo: Unroll Macro: Bucket -> Node
(defstruct (Bucket
	    (:predicate bucket-)
	    (:constructor make-toplevel-bucket (indices &aux (tree-level 0)))
	    (:constructor make-sub-bucket (indices tree-level))
	    )
  ;; (C 0 :type fixnum) 
  (tree-level tree-level :type fixnum)
  (index 0 :type fixnum) ;; split-index
  (threshold  0.0 :type single-float) ;; = split-val
  (threshold-candidates nil) ;; 0~dim
  (next-nodes nil :type list)
  (indices indices :type list)) ;; The list of indices of A which current bucket posses (C, D), D= 1 3 5 10 2 ... Bucketが管轄するDのIndex

(defmethod col-variances ((bucket Bucket) subspace &aux (N (length (bucket-indices bucket))))
  (declare (optimize (speed 3))
	   (type matrix subspace))
  (with-view (s* subspace `(:indices ,@(bucket-indices bucket)) t)
    (with-caches ((ex2 (shape s*) :place-key :col-variances1)
		  (ex  (shape s*) :place-key :col-variances2)
		  (result `(1 ,(second (shape s*))) :place-key :sum-out1))
      (%fill result 0.0)
      ;; Sum(E[x^2] - E[x]^2, axis=0)
      (%move s* ex2)
      (%move s* ex)
      (%square ex2)
      (%scalar-div ex2 N)
      (%scalar-div ex N)
      (%square ex)
      (%subs ex2 ex)
      (%sum ex2 :out result :axis 0)
      result)))

(defun optimize-split-thresholds! (bucket index)
  "Pick up index-th threshold-candiates, and use it as bucket's threshold."
  (declare (optimize (speed 3))
	   (type bucket bucket)
	   (type fixnum index))

  (when (bucket-next-nodes bucket)
    (setf (bucket-threshold bucket) (nth index (reverse (bucket-threshold-candidates bucket)))))

  (let ((buckets (bucket-next-nodes bucket)))
    (when buckets
      (optimize-split-thresholds! (car buckets) index)
      (optimize-split-thresholds! (cdr buckets) index)))
  nil)

(defun optimize-bucket-splits! (bucket best-dim subspace)
  "Splits bucket's binary-tree
split-val dim"
  (declare (optimize (speed 3))
	   (type bucket bucket)
	   (type matrix subspace)
	   (type fixnum best-dim))

  ;; if bucket-nodes = nil -> Create new
  ;; if t  -> Optimize the old one
  ;; Add: if indices = nil?

  (flet ((create-new-bucket (points)
	   (make-sub-bucket points (1+ (bucket-tree-level bucket))))
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

	;; Note: Having avoided using maddness-hash but using cons to express tree-structure, I am wondering this semantics below is currect?
	
	(%>  x split-val :out mask)     ;; left
	(%<= x split-val :out not-mask) ;; right

	(setq left-side-points  (make-tflist-indices mask))
	(setq right-side-points (make-tflist-indices not-mask))
	
	;; When left side child is supposed to be nil...?
	;; either is filled with copy of bucket
	(when (= (the single-float (%sumup mask)) 0.0)
	  (setq left-side-points (bucket-indices bucket)))

	;; When right side child is supposed to be nil...?
	(when (= (the single-float (%sumup not-mask)) 0.0)
	  (setq left-side-points (bucket-indices bucket)))

	
	(if (null (bucket-next-nodes bucket))
	    ;; If bucket is the end of node...
	    ;; => Creates a new bucket-tree.
	    (progn
	      (setf (bucket-next-nodes bucket)
		    (cons (create-new-bucket left-side-points)
			  (create-new-bucket right-side-points)))
	      nil)
	    ;; Otherwise -> Go deeper and update nodes.
	    (let ((nodes (bucket-next-nodes bucket)))
	      ;; Update Current Bucket -> Go Deeper

	      ;; Update thresholds?
	      (setf (bucket-indices (car nodes)) left-side-points)
	      (setf (bucket-indices (cdr nodes)) right-side-points)

	      ;; Update Left-side
	      (optimize-bucket-splits!
	       (car nodes)
	       best-dim
	       subspace)

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

		  (print buckets)
		  (optimize-split-thresholds! buckets best-dim)
		  (optimize-bucket-splits!    buckets best-dim subspace)
		  (print buckets)
		  ;;(print buckets)
		  )))))
	buckets))))

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


;; (optimize-params bucket)
(defun optimal-val-splits! (subspace bucket total-losses d dim tree-level)
  "Tree-LevelまでBucketを探索してcompute-optimal-val-splitsする

与えれたdimでbinary-treeを学習/loss var (bucket isn't modified.)
split-dimsを決定

Return:
   - (values best-val early-stopping-p)
"
  (declare (optimize (speed 3))
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
	    (push split-val (bucket-threshold-candidates bucket))
	    ;; Judge early-stoppig-p
	    (if (= d 0)
		nil
		(%all?
		 (%satisfies
		  (view total-losses t `(0 ,d))
		  #'(lambda (x) (< loss-d* (the single-float x)))))))))
      (let ((next-nodes (bucket-next-nodes bucket)))
	;; Explore nodes untill reach tree-level

	(when (null next-nodes)
	  (error "optimal-val-splits! Couldn't find any buckets."))

	(let ((res1 (optimal-val-splits! subspace (car next-nodes) total-losses d dim tree-level))
	      (res2 (optimal-val-splits! subspace (cdr next-nodes) total-losses d dim tree-level)))
	  (or res1 res2)))))

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

  (when (null (bucket-indices bucket))
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
      
      (cumulative-sse! (view x `(:indices ,@x-sort-indices))     x-head)
      (cumulative-sse! (view x `(:indices ,@x-sort-indices-rev)) x-tail)

      ;; x-head = sses-head, x-tail = sses-tail
      ;; losses <- sses-head 
      ;; losses[1:N-1] <- losses[1:N-1] + sses_tail[2:N]

      ;; Maybe this if clause is not necessary.
      
      (if (= (the fixnum (car (shape x-head))) 1) ;; Check if N >= 1.
	  (%adds x-head x-tail)
	  (%adds (view x-head `(0 -1)) (view x-tail `(1 :~))))
      
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
	  
	  (values (/ (+ (the single-float (%sumup c1))
			(the single-float (%sumup c2)))
		     2.0)
		  (the single-float (%sumup (view x-head best-idx)))))))))


(defun cumulative-sse! (xp
			cumsses
			&aux
			  (N (car    (shape xp)))
			  (D (second (shape xp)))
			  (dtype     (dtype xp)))
  "Algorithm 4 Cumulative SSE. (Computes SSE Loss)

   Input: X [N D]
          out - the matrix to be overwritten with result. If nil, The function allocates a new matrix.
   Output: Cumsses [N D]"
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

(defun splits-buckets (bucket)

)


(defun test (&key (p 0.8))
  ;; How tall matrix is, computation time is constant.
  (let ((matrix (matrix `(256 32))))
    (%index matrix #'(lambda (i)
		       (if (< (random 1.0) p)
			   (random 1.0)
			   0.0)))
    ;; (sb-ext:gc :full t)
    ;;(sb-profile:profile "CL-XMATRIX")
    (time (init-and-learn-offline matrix 4))
    ;;(sb-profile:report)
    ;;(sb-profile:unprofile "CL-XMATRIX")

    (free-mat matrix)
    ))
