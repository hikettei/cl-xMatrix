
(in-package :cl-xmatrix)

(defun load-openblas () ; Todo: *xmatrix-environment* support it by c-level
  (load-foreign-library "/usr/local/Cellar/openblas/0.3.22/lib/libblas.dylib"))
