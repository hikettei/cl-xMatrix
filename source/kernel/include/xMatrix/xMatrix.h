#pragma once

#include <stdint.h>

// uint8
typedef uint8_t minifloat;
// 16bit float.

// ref: https://github.com/ggerganov/ggml/blob/553929cf771634fc29b0700967a0621d56647f09/include/ggml/ggml.h#L186

#ifdef __ARM_NEON
// we use the built-in 16-bit float type
typedef __fp16 fp16_t;
#else
typedef uint16_t fp16_t;
#endif

// 32bit float
typedef float single_float;

// cpu-identifications
int cpu_has_sse(void);
int cpu_has_avx(void);
int cpu_has_avx2(void);
int cpu_has_avx512(void);

struct ViewInstruction {
  int offset;
  int actualoffset;
  int stride2;
  int stride1;
  int offset2;
  int offset1;
  int m;
  int n;
  int broadcast2;
  int broadcast1;
};

// element-wise mathematical functions

void fp32_abs(const struct ViewInstruction, single_float*);
void fp16_abs(const struct ViewInstruction, single_float*);

void fp32_copy(const struct ViewInstruction, single_float*, single_float*);
