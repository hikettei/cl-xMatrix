
(in-package :cl-user)


(defpackage :cl-xmatrix
  (:use :cl :cffi) ;; To Add: GC by trivial-garbage or something
  (:export :load-xmatrix)
  (:export
   #:matrix
   #:free-mat)

  (:export
   #:%abs)

  (:export
   #:with-pointer-barricade)
  
  ;; View APIs
  (:export
   #:view
   #:with-view
   #:with-views
   #:call-with-visible-area
   #:with-view-object
   ))

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
