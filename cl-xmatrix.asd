
(in-package :cl-user)

(defpackage cl-xmatrix-asd
  (:use :cl :asdf :uiop))

(in-package :cl-xmatrix-asd)

(asdf:defsystem :cl-xmatrix
  :author "hikettei"
  :licence "MIT"
  :description "matrix operation library for common lisp"
  :pathname "source"
  :serial t
  :depends-on (:cffi :cffi-libffi #:trivial-garbage #:bordeaux-threads #:alexandria #:cl-annot)
  :components ((:file "package")
	       (:file "matrix")
	       (:file "render")
	       (:file "view")
	       (:file "quantize")
	       (:file "cache")

	       (:file "apis/arithmetic")
	       (:file "apis/mathematical")
	       (:file "apis/operations")

	       (:file "distribution/random")
	       
	       (:file "amm/package")
	       (:file "amm/mithral")
	       ;;(:file "mha")
	       )
  :in-order-to ((test-op (test-op cl-xmatrix/test))))

(defpackage cl-xmatrix-test
  (:use :cl :asdf :uiop))

(in-package :cl-xmatrix-test)

(defsystem :cl-xmatrix/test
  :author "hikettei"
  :licence "MIT"
  :description "test for xmatrix"
  :pathname "t"
  :serial t
  :depends-on (:cl-xmatrix :fiveam)
  :components ((:file "package")
	       (:file "utils")
	       (:file "view")
	       (:file "arithmetic"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :test)))
