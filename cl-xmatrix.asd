
(in-package :cl-user)

(asdf:defsystem :cl-xmatrix
  :author "hikettei"
  :licence "MIT"
  :description "matrix operation library for common lisp"
  :pathname "source"
  :serial t
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "matrix")
	       (:file "view")))
