
(in-package :cl-xmatrix)

;; Facet API Handling multiple facet of array.

;;
;;
;; CFFI Pointer <-> Simple-Array
;;                  Array
;;                  
;;

;; Excepted Workflow:
;;
;; a = matrix(hogehoge) One initializes a new matrix with CFFI Pointer
;; Work with SIMD/C, If complicated operation needed, write extensions in Lisp.
;; No Overheads between a<->SIMD<->Lisp Kernel
;;

;; Using CLOS
;; 目標 AMM/MaddnessのEncoding時間の短縮
;; Export MaddnessMatmul.

;; 扱うデータは全て一次元の行列となる
;; (defmacro subscript (hoge) ~)

;; First Class <- Simple-Array
;; -> Calling CFFI... with foreign-ptr
;; -> Writing Kernel With ... simple-array

;; 設計を考える

;; AbstractMatrix

;; -> define-backend matrix (with generics)
;; Handling in multiple devices, and 微分可能に

(defun cl-array->foreign-ptr (array dtype)
  (declare (type simple-array array)
	   (ignorable dtype))
  #+sbcl
  (sb-sys:vector-sap (sb-ext:array-storage-vector array))
  #-(or sbcl)
  (error "TODO")) ;;Add: Allocate Array -> CFFI



