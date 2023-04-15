
# xMatrix

My playground for matrix operations. No guarantee of code.

In my spare time, I might collect APIs and write tests, and make it library.

If so, cl-xMatrix can became:

- The only library (for Common Lisp) which supports: FP16 Quantization, view-function. (but only cpus)
- The library which includes a lot of the AMM methods.

# Features

Ease of use and processing transparency first, speed comes second.

## View Function

Provides view function that are fully equivalent to Numpy and Torch's View. Calling view itself doesn't make any copies, but treated as a matrix with a different shape.

Also, view function includes these external operations:

`:indices 1 2 3...`


## CFFI Pointer Array


## Connection between CFFI Pointer and Lisp Array


# Specifications

## (To be) Supported DataType

- Minifloat (8bit float)
- FP16 (16bit float)
- FP32 (single-float)

# Requirements

- CPU which supports AVX (2012~)

# Roadmap

- Accelerating with OpenBLAS (only gemm but uint16?)

- Quantize and 4bit (FP16 is done, int4 is really necessary?...)

- Lisp-level multi-threading support.

- Lisp-level view function support (almost done but error check)


# Todo:

element-wiseな関数を一個作ってviewのサポート
それに倣って他の関数定義

1. 一引数の関数(abs)を一旦定義

2. 2変数の要素ごとの関数(add)を一つだけ定義

3. 二つの関数をFP16, FP8, fp32に対応

4. Lisp-levelのBroadcasting

5. 4bit量子化に挑戦

6. 他の関数をSIMD化してみる(AVX, AVX2)

