
(in-package :cl-xmatrix)

(defun load-openblas () ; Todo: *xmatrix-environment*
  (load-foreign-library "/usr/local/Cellar/openblas/0.3.22/lib/libblas.dylib"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-openblas))
