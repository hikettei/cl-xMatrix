
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.maddness
  (:use :cl :cl-xmatrix)
  (:export
   ))

(in-package :cl-xmatrix.amm.maddness)

;; This is the my reimplementation of Maddness

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

		  ;;(print buckets)
		  (optimize-split-thresholds! buckets best-dim)
		  (optimize-bucket-splits!    buckets best-dim subspace)
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
      
      (cumulative-sse! (view x `(:indices ,@x-sort-indices))     x-head)
      (cumulative-sse! (view x `(:indices ,@x-sort-indices-rev)) x-tail)

      ;; x-head = sses-head, x-tail = sses-tail
      ;; losses <- sses-head 
      ;; losses[1:N-1] <- losses[1:N-1] + sses_tail[2:N]

      ;; Maybe this if clause is not necessary.
      
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


#|
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

(defun test (&key (p 0.8))
  ;; How tall matrix is, computation time is constant.
  (let ((matrix (matrix `(1000 64))))
    (%index matrix #'(lambda (i)
		       (if (< (random 1.0) p)
			   (random 1.0)
			   0.0)))
    ;;(sb-ext:gc :full t)
    ;;(sb-profile:profile "CL-XMATRIX.AMM.MADDNESS")
    (time (init-and-learn-offline matrix 4))
    ;;(sb-profile:report)
    ;;(sb-profile:unprofile "CL-XMATRIX.AMM.MADDNESS")

    (free-mat matrix)
    ))
