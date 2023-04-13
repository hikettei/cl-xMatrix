
(in-package :cl-xmatrix)

;; Wrappers for Mithral

void mithral_encode(const int8_t *X, int64_t nrows, int ncols,
                    const uint32_t *splitdims, const int8_t *all_splitvals,
			  int ncodebooks, uint8_t *out);

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
		      (:pointer :uint8) out-pointer))
    (:float

     )))



(defun test-mithral ()
  (let ((x (quantize-matrix (matrix `(100 100))))
	(nrows 100)
	(ncols 100)
	(splitdims (matrix `(100 100) :dtype :int))
	(all-splitvals (quantize-matrix (matrix `(100 100))))
	(out (quantize-matrix (matrix `(100 100)))))
    (mithral-encode (matrix-vec x)
		    nrows
		    ncols
		    (matrix-vec splitdims)
		    (matrix-vec all-splitvals)
		    4
		    (matrix-vec out)
		    :int8)))

(defun offline-learning ())
(defmacro with-mithral-learning ())



