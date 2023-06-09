
(in-package :cl-user)


(defpackage :cl-xmatrix
  (:use :cl :cffi)

  ;; Utils for shared library
  (:export
   #:load-xmatrix
   #:cpu-information)

  ;; Caching
  (:export
   #:with-internal-system-caching
   #:with-cache
   #:with-caches)

  (:export
   #:*available-dtypes*)
  
  ;; Matrix APIs
  (:export
   #:matrix
   #:shape
   #:dims
   #:convert-into-lisp-array
   #:free-mat
   #:dtype
   #:1d-mat-aref
   #:coerce-to-dtype
   #:matrix-vec)

  ;; Mat Pointer API
  (:export
   #:with-pointer-barricade)
  
  ;; View APIs
  (:export
   #:view
   #:with-view
   #:with-views
   #:incf-view!

   #:incf-offsets!
   #:reset-offsets!
   
   #:call-with-visible-area
   #:with-view-object
   #:with-broadcasting

   #:Indexing-Error
   #:View-Indexing-Error)

  ;; Arithmetic
  (:export
   #:%adds
   #:%subs
   #:%muls
   #:%divs

   #:%scalar-add
   #:%scalar-sub
   #:%scalar-mul
   #:%scalar-div

   #:%fill

   #:%move
   #:%copy)


  ;; Mathematical
  (:export
   #:%abs

   #:%square
   #:%log
   #:%exp
   #:%power
   #:%expt

   ;; More will be added...
   )

  ;; Reductions
  (:export
   #:%sum
   #:%sumup)

  ;; Compares
  (:export
   #:%filter
   #:%index
   #:%satisfies
   #:%compare
   #:%all?
   #:%or?
   )

  )

;; Memo
;; :shadowing-import-from ,and then cl-xmatrix::+
;; Intermixied APIs for arithmetic operations: cl-xmatrix::+ と cl-xmatrix::add (+はalias for add).

(in-package :cl-xmatrix)

(defun xmatrix-pathname-darwin ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libxMatrix.dylib"))

(defun libmaddness-pathname-darwin ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libMithral.dylib"))

(defun xmatrix-pathname-darwin ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libxMatrix.dylib"))

(defun libmaddness-pathname-darwin ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libMithral.dylib"))

(defun xmatrix-pathname-windows ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libxMatrix.dll"))

(defun libmaddness-pathname-windows ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libMithral.dll"))

(defun xmatrix-pathname-linux ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libxMatrix.so"))

(defun libmaddness-pathname-linux ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libMithral.so"))

;; Fixme: define-foreign-library and use-foreign-lib...

(defun load-xmatrix ()
  (let ((libpath (xmatrix-pathname-darwin))
	(libpath1 (libmaddness-pathname-darwin)))
    
    (handler-case (load-foreign-library libpath)
      (cffi:load-foreign-library-error (c)
	(warn "cl-xmatrix couldn't find libxMatrix.dylib. Please compile it with `lake build` and try again.")
	(error c)))
    
    (handler-case (load-foreign-library libpath1)
      (cffi:load-foreign-library-error (c)
	(declare (ignore c))
	(warn "cl-xmatrix couldn't find libMithral.dylib")))
    t))

(defun cpu-information (&key (stream t))
  "Displays cpu-information, TODO: for more details"
  (format stream "cpu_has_avx:   ~a~%cpu_has_avx2:  ~a~%cpu_has_avx512 ~a~%"
	  (foreign-funcall "cpu_has_avx2" :int)
	  (foreign-funcall "cpu_has_avx2" :int)
	  (foreign-funcall "cpu_has_avx512" :int)))

(load-xmatrix)
