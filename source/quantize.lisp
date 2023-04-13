
(in-package :cl-xmatrix)


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

