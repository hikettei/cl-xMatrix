#pragma once

#include <stdint.h>

// uint8
typedef uint8_t minifloat;
// 16bit float.
typedef __fp16 fp16_t;
// 32bit float
typedef float single_float;

// cpu-identifications
int cpu_has_sse(void);
int cpu_has_avx(void);
int cpu_has_avx2(void);
int cpu_has_avx512(void);

struct ViewInstruction {
  int offset;
  int stride2;
  int stride1;
  int offset2;
  int offset1;
  int m;
  int n;
};


// element-wise mathematical functions

void fp32_abs(const struct ViewInstruction, single_float*);

