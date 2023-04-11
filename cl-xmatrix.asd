
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
  :depends-on (:cffi)
  :components ((:file "package")
	       (:file "matrix")
	       (:file "view"))
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
  :components ((:file "package"))
  :perform (test-op (o s)
		    (symbol-call :fiveam :run! :test)))