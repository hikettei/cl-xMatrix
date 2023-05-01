
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
The function init-and-learn-offline clusters the prototypes and computes the encoding function g(a).

Assertion: N must be divided by C (while the original impl doesn't impose it.)

========================================================================
   D         D
  +++     C +++ <- N*D Matrix is disjointed into C*D Matrix
N +++ ->     D
  +++     C +++
            ... x (N//C)
========================================================================
Input:
  - a-offline The Training Matrix.
  - C fixnum  The Parameter Variable, C.
Return:
  - (values ) 
"
  (declare (optimize (speed 3))
	   (type matrix a-offline)
	   (type fixnum N D C K))

  (assert (= (mod N C) 0) nil "Assertion Failed with (= (mod N C) 0). N=~a C=~a" N C)

  ;; with-view: Cut out the shape of matrix.
  ;; The visible-area is adjusted by modifying offsets. (the elements[0~offsets] must be continuous in memory.)
  ;;    D        D
  ;;   +++    C +-- <- offset=0
  ;; N +++ =>   +--
  ;;   +++      +--
  ;;
  ;; Symbols: + ... Visible / - ... Invisible

  ;; all-prototypes (C, K, D)

  (let ((all-prototypes (or all-prototypes-out
			    t ;;del
			    (matrix `(,C ,K ,D) :dtype (dtype a-offline))))
	(step (/ D C)))
    
    (with-view (a-offline* a-offline t `(0, C))
      (loop for i fixnum upfrom 0 below C
	    do (progn

		 (incf-offset! matrix t 3)
		 )

      ))))

;; B(t, i)
;; Each Bucket possess: tree-level, index, threshold, next-nodes
;; compare: subspace[index] > threshold

;; Todo Pprint
;; Todo: Unroll Macro: Bucket -> Node
(defstruct (Bucket
	    (:constructor make-toplevel-bucket (bucket-indices &aux (tree-level 0)))
	    ;;(:constructor make-subbucket ())
	    )
  ;; (C 0 :type fixnum) 
  (tree-level tree-level :type fixnum)
  (index 0 :type fixnum) ;; split-index
  (threshold  0.0        :type single-float)
  (next-nodes nil :type list)
  (bucket-indices bucket-indices :type list)) ;; The list of indices of A which current bucket posses (C, D), D= 1 3 5 10 2 ... Bucketが管轄するDのIndex

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
 - subspace Matrix
 - C, D     Fixnum
 - nsplits The number of training, the paper has it that setting 4 is always the best.

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
	  (optimal-val-splits! buckets)

	  
	  
	  
      
	  )))))

(defun test ()
  (let ((matrix (matrix `(128 16))))
    (%index matrix #'(lambda (i) (+ i 0.0)))
    (time (init-and-learn-offline matrix 4))))
