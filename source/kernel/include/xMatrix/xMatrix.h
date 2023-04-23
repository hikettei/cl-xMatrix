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

typedef uint8_t fp8_t;

// 32bit float
typedef float single_float;

// cpu-identifications
int cpu_has_sse(void);
int cpu_has_avx(void);
int cpu_has_avx2(void);
int cpu_has_avx512(void);

single_float* fp32_allocate_aligned_mat(int);
fp16_t* fp16_allocate_aligned_mat(int);
fp8_t* fp8_allocate_aligned_mat(int);
int* int_allocate_aligned_mat(int);

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
void fp16_abs(const struct ViewInstruction, fp16_t*);
void fp8_abs(const struct ViewInstruction, fp8_t*);
void int_abs(const struct ViewInstruction, int*);

void fp32_sin(const struct ViewInstruction, single_float*);
void fp16_sin(const struct ViewInstruction, fp16_t*);
void fp8_sin(const struct ViewInstruction, fp8_t*);
void int_sin(const struct ViewInstruction, int*);

void fp32_cos(const struct ViewInstruction, single_float*);
void fp16_cos(const struct ViewInstruction, fp16_t*);
void fp8_cos(const struct ViewInstruction, fp8_t*);
void int_cos(const struct ViewInstruction, int*);

void fp32_tan(const struct ViewInstruction, single_float*);
void fp16_tan(const struct ViewInstruction, fp16_t*);
void fp8_tan(const struct ViewInstruction, fp8_t*);
void int_tan(const struct ViewInstruction, int*);


void fp32_copy(const struct ViewInstruction, const struct ViewInstruction, single_float*, single_float*);
void fp16_copy(const struct ViewInstruction, const struct ViewInstruction, fp16_t*, fp16_t*);
void fp8_copy(const struct ViewInstruction, const struct ViewInstruction, fp8_t*, fp8_t*);
void int_copy(const struct ViewInstruction, const struct ViewInstruction, int*, int*);

// Arithmetic Operations
void fp32_add(const struct ViewInstruction, const struct ViewInstruction, single_float*, single_float*);
void fp16_add(const struct ViewInstruction, const struct ViewInstruction, fp16_t*, fp16_t*);
void fp8_add(const struct ViewInstruction, const struct ViewInstruction, fp8_t*, fp8_t*);
void int_add(const struct ViewInstruction, const struct ViewInstruction, int*, int*);



void fp32_sub(const struct ViewInstruction, const struct ViewInstruction, single_float*, single_float*);
void fp16_sub(const struct ViewInstruction, const struct ViewInstruction, fp16_t*, fp16_t*);
void fp8_sub(const struct ViewInstruction, const struct ViewInstruction, fp8_t*, fp8_t*);
void int_sub(const struct ViewInstruction, const struct ViewInstruction, int*, int*);


void fp32_mul(const struct ViewInstruction, const struct ViewInstruction, single_float*, single_float*);
void fp16_mul(const struct ViewInstruction, const struct ViewInstruction, fp16_t*, fp16_t*);
void fp8_mul(const struct ViewInstruction, const struct ViewInstruction, fp8_t*, fp8_t*);
void int_mul(const struct ViewInstruction, const struct ViewInstruction, int*, int*);



void fp32_div(const struct ViewInstruction, const struct ViewInstruction, single_float*, single_float*);
void fp16_div(const struct ViewInstruction, const struct ViewInstruction, fp16_t*, fp16_t*);
void fp8_div(const struct ViewInstruction, const struct ViewInstruction, fp8_t*, fp8_t*);
void int_div(const struct ViewInstruction, const struct ViewInstruction, int*, int*);


void fp32_scalar_add(const struct ViewInstruction, single_float*, single_float);
void fp16_scalar_add(const struct ViewInstruction, fp16_t*, fp16_t);
void fp8_scalar_add(const struct ViewInstruction, fp8_t*, fp8_t);
void int_scalar_add(const struct ViewInstruction, int*, int);

void fp32_scalar_mul(const struct ViewInstruction, single_float*, single_float);
void fp16_scalar_mul(const struct ViewInstruction, fp16_t*, fp16_t);
void fp8_scalar_mul(const struct ViewInstruction, fp8_t*, fp8_t);
void int_scalar_mul(const struct ViewInstruction, int*, int);

void fp32_fill(const struct ViewInstruction, single_float*, single_float);
void fp16_fill(const struct ViewInstruction, fp16_t*, fp16_t);
void fp8_fill(const struct ViewInstruction, fp8_t*, fp8_t);
void int_fill(const struct ViewInstruction, int*, int);


