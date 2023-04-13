
(in-package :cl-xmatrix)

;; MultiHeadAttention

;; Required: Softmax: Exp, Sum, Broadcasted Div. Scalar Mul Matmul, Traspose. Reshape,
;; Note: Every source words are quantized and created lut.

;; Todo: Make benchmarks and Assessment of accuracy
;; (defclass MultiHeadAttention ...
