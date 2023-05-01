
(in-package :cl-user)

(defpackage :cl-xmatrix.amm.maddness
  (:use :cl :cl-xmatrix)
  (:export
   #:init-and-learn-mithral))

(in-package :cl-xmatrix.amm.maddness)

;; This is the my reimplementation of Maddness



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
		 (learn-binary-tree-splits a-offline* C D)
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
	    ;;(:constructor make-subbucket ())
	    )
  ;; (C 0 :type fixnum) 
  (tree-level tree-level :type fixnum)
  (index 0 :type fixnum) ;; split-index
  (threshold  0.0        :type single-float)
  (next-nodes nil :type list)
  (indices indices :type list)) ;; The list of indices of A which current bucket posses (C, D), D= 1 3 5 10 2 ... Bucketが管轄するDのIndex

;; (defun maddness-hash ())

(defun learn-binary-tree-splits (subspace C D &key (nsplits 4) (verbose t))
  "
The function learn-binary-tree-splits computes maddness-hash given subspace X.

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
	   (type fixnum C D nsplits)
	   (type boolean verbose))

  (let ((buckets (make-toplevel-bucket
		  ;; B(1, 1) possess all the elements in the subspace.
		  (loop for i fixnum upfrom 0 below D
			collect i))))

    ;; Utils
    (labels ((col-losses ()
	       ))

      ;; Utils
      (macrolet ((maybe-print (object &rest control-objects)
		   `(when verbose (format t ,object ,@control-objects))))
	
	;; Training
	(dotimes (nth-split nsplits)
	  (maybe-print "== (~a/~a)Training Binary Tree Splits =========" (1+ nth-split) nsplits)

	  ;; AddHere: Compute losses by columns

	  ;; d=[Largest-Loss-Axis, ..., Smallest-Loss-Axis]
	  (optimal-val-splits! buckets 0)

	  ;; loop for i in N. <- NをLossでSortする。

	  
	  
	  
      
	  )))))


(defun sort-rows-based-on-col (matrix indices)
  
  )

(defun optimal-val-splits (subtype bucket index tree-level
			   &aux
			     (N (first (shape subtype)))
			     (D (second (shape subtype))))
  "Tests all possible thresholds to find one minimizing B(t, i).
Ref: Appendix C, Algorithm 3, Optimal Split Threshold Within a Bucket.

"
  (declare (optimize (speed 3))
	   (type matrix subtype)
	   (type Bucket bucket)
	   (type fixnum index tree-level))

  (with-caches ((x-head `(,N ,D) :dtype (matrix-dtype subtype) :place-key :C1)
		(x-tail `(,N ,D) :dtype (matrix-dtype subtype) :place-key :C2))

    (let* ((indices (bucket-indices bucket))
	   (x (view subtype t `(:indices ,@indices)))
	   (x-sort-indices     (sort-rows-based-on-col x indices))
	   (x-sort-indices-rev (reverse x-sort-indices)))
      (declare (type list x-sort-indices))
      
      (cumulative-sse! (view x t `(:indices ,@x-sort-indices))     x-head)
      (cumulative-sse! (view x t `(:indices ,@x-sort-indices-rev)) x-tail)
      

      )))


(defun cumulative-sse! (x
			cumsses
			&aux
			  (N (car    (shape x)))
			  (D (second (shape x)))
			  (dtype     (dtype x)))
  "Algorithm 4 Cumulative SSE. (Computes SSE Loss)

   Input: X [N D]
          out - the matrix to be overwritten with result. If nil, The function allocates a new matrix.
   Output: Cumsses [N D]"
  (declare (optimize (speed 3) (safety 0))
	   (type index N D)
	   (type matrix x cumsses))
  
  (with-caches ((cumX-cols `(1 ,D) :dtype dtype :place-key  :cumsse-col1)
		(cumX2-cols `(1 ,D) :dtype dtype :place-key :cumsse-col2))
    (%fill cumX-cols 0.0)
    (%fill cumX2-cols 0.0)
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

(defun splits-buckets (bucket)

)


(defun test ()
  (let ((matrix (matrix `(128 32))))
    (%index matrix #'(lambda (i) (+ i 0.0)))
    (time (init-and-learn-offline matrix 4))))
