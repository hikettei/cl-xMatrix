
# xMatrix

My playground for matrix operations.

# Features

## View Function

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

- Accelerating with OpenBLAS

- Quantize and 4bit

- Lisp-level multi-threading support

- Lisp-level view function support


# Todo:

element-wiseな関数を一個作ってviewのサポート
それに倣って他の関数定義

1. 一引数の関数(abs)を一旦定義

2. 2変数の要素ごとの関数(add)を一つだけ定義

3. 二つの関数をFP16, FP8, fp32に対応

4. Lisp-levelのBroadcasting

5. 4bit量子化に挑戦

6. 他の関数をSIMD化してみる(AVX, AVX2)