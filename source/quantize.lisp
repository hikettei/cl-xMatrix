
(in-package :cl-xmatrix)

;; APIs for Dtypes

(defcfun "convert_fp32_into_fp16" (:pointer :uint8)
  (size :int)
  (x (:pointer :float)))


(defcfun "convert_fp16_into_fp32" (:pointer :float)
  (size :int)
  (x (:pointer :uint8)))

(defcfun "convert_fp32_into_fp16_within_view" (:pointer :uint8)
  (view (:struct ViewInstruction))
  (x (:pointer :float)))


(defcfun "convert_fp16_into_fp32_within_view" (:pointer :float)
  (view (:struct ViewInstruction))
  (x (:pointer :uint8)))

(defun fp32->fp16 (size pointer)
  "Creates a new matrix whose dtype is uint8, the old matrix is freed.
Matrix is given by (matrix-vec mat)"
  (prog1
      (convert-fp32-into-fp16 size pointer)
    ;(free-mat pointer)
    ))

(defun fp16->fp32 (size pointer)
  (prog1
      (convert-fp16-into-fp32 size pointer)
    ;(free-mat pointer)
    ))

(defun vec-quantize-into (pointer shape dtype)
  (case dtype
    (:fp16
     (fp32->fp16 (apply #'* shape) pointer))
    (:float pointer)))

(defun vec-dtype-quantized (dtype)
  (case dtype
    (:fp16 :uint16)
    (T dtype)))
