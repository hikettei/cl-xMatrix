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

// element-wise mathematical functions
