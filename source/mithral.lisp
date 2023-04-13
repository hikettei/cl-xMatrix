
(in-package :cl-xmatrix)

;; Wrappers for Mithral

(defcfun ("mithral_encode_int8_t" mithral-encode) :void
  (X-pointer (:pointer :uint8))
  (nrows :int)
  (ncols :int)
  (splitdims-pointer (:pointer :int32))
  (all-splitvals-pointer (:pointer :uint8))
  (ncodebooks :int)
  (out-pointer (:pointer :uint8)))
#|
(defun mithral-encode (X-pointer nrows ncols splitdims-pointer all-splitvals-pointer ncodebooks out-pointer dtype)
  (case dtype
    (:int8
     (foreign-funcall "mithral_encode_int8_t"
		      (:pointer :uint8) X-pointer
		      :int nrows
		      :int ncols
		      (:pointer :int32) splitdims-pointer
		      (:pointer :uint8) all-splitvals-pointer
		      :int ncodebooks
		      (:pointer :uint8) out-pointer
		      :void))
    (:float

     )))
|#

(defun test-mithral ()
  (let ((x (quantize-matrix (matrix `(100 100))))
	(nrows 100)
	(ncols 100)
	(splitdims (matrix `(100 100) :dtype :int))
	(all-splitvals (quantize-matrix (matrix `(100 100))))
	(out (quantize-matrix (matrix `(100 100)))))
    (mithral-encode
                   (matrix-vec x)
		    nrows
		    ncols
		    (matrix-vec splitdims)
		    (matrix-vec all-splitvals)
		    4
		    (matrix-vec out))))

(defun offline-learning ())
(defmacro with-mithral-learning ())



