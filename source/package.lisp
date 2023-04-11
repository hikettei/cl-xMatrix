
(in-package :cl-user)


(defpackage :cl-xmatrix
  (:use :cl :cffi) ; To Add: GC by trivial-garbage or something
  (:export
   #:matrix
   #:free-mat))

(in-package :cl-xmatrix)

(defun get-shared-xmatrix-pathname ()
  (asdf:system-relative-pathname "cl-xmatrix" "./source/kernel/libxMatrix.dylib"))

(defun xmatrix-built-p ()
  t)

; Fixme: define-foreign-library and use-foreign-lib...
(defun load-xmatrix ()
  (let ((libpath (get-shared-xmatrix-pathname)))
    ; Add: conditions and handler-case
    (load-foreign-library libpath)))

(defun cpu-information (&key (stream t))
  "Displays cpu-information, TODO: for more details"
  (format stream "~a"
	  (foreign-funcall "cpu_has_avx2" :int)))

(load-xmatrix)
