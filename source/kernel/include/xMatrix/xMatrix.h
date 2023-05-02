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
  int broadcast2; // 0 or 1
  int broadcast1; // 0 or 1
};



void fp32_copy(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*);
void fp16_copy(const struct ViewInstruction*, const struct ViewInstruction*, fp16_t*, fp16_t*);
void fp8_copy(const struct ViewInstruction*, const struct ViewInstruction*, fp8_t*, fp8_t*);
void int_copy(const struct ViewInstruction*, const struct ViewInstruction*, int*, int*);

// Arithmetic Operations
void fp32_add(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*);
void fp16_add(const struct ViewInstruction*, const struct ViewInstruction*, fp16_t*, fp16_t*);
void fp8_add(const struct ViewInstruction*, const struct ViewInstruction*, fp8_t*, fp8_t*);
void int_add(const struct ViewInstruction*, const struct ViewInstruction*, int*, int*);



void fp32_sub(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*);
void fp16_sub(const struct ViewInstruction*, const struct ViewInstruction*, fp16_t*, fp16_t*);
void fp8_sub(const struct ViewInstruction*, const struct ViewInstruction*, fp8_t*, fp8_t*);
void int_sub(const struct ViewInstruction*, const struct ViewInstruction*, int*, int*);


void fp32_mul(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*);
void fp16_mul(const struct ViewInstruction*, const struct ViewInstruction*, fp16_t*, fp16_t*);
void fp8_mul(const struct ViewInstruction*, const struct ViewInstruction*, fp8_t*, fp8_t*);
void int_mul(const struct ViewInstruction*, const struct ViewInstruction*, int*, int*);



void fp32_div(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*);
void fp16_div(const struct ViewInstruction*, const struct ViewInstruction*, fp16_t*, fp16_t*);
void fp8_div(const struct ViewInstruction*, const struct ViewInstruction*, fp8_t*, fp8_t*);
void int_div(const struct ViewInstruction*, const struct ViewInstruction*, int*, int*);


void fp32_scalar_add(const struct ViewInstruction*, single_float*, single_float);
void fp16_scalar_add(const struct ViewInstruction*, fp16_t*, fp16_t);
void fp8_scalar_add(const struct ViewInstruction*, fp8_t*, fp8_t);
void int_scalar_add(const struct ViewInstruction*, int*, int);

void fp32_scalar_mul(const struct ViewInstruction*, single_float*, single_float);
void fp16_scalar_mul(const struct ViewInstruction*, fp16_t*, fp16_t);
void fp8_scalar_mul(const struct ViewInstruction*, fp8_t*, fp8_t);
void int_scalar_mul(const struct ViewInstruction*, int*, int);

void fp32_scalar_sub(const struct ViewInstruction*, single_float*, single_float);
void fp16_scalar_sub(const struct ViewInstruction*, fp16_t*, fp16_t);
void fp8_scalar_sub(const struct ViewInstruction*, fp8_t*, fp8_t);
void int_scalar_sub(const struct ViewInstruction*, int*, int);

void fp32_scalar_div(const struct ViewInstruction*, single_float*, single_float);
void fp16_scalar_div(const struct ViewInstruction*, fp16_t*, fp16_t);
void fp8_scalar_div(const struct ViewInstruction*, fp8_t*, fp8_t);
void int_scalar_div(const struct ViewInstruction*, int*, int);

void fp32_fill(const struct ViewInstruction*, single_float*, single_float);
void fp16_fill(const struct ViewInstruction*, fp16_t*, fp16_t);
void fp8_fill(const struct ViewInstruction*, fp8_t*, fp8_t);
void int_fill(const struct ViewInstruction*, int*, int);

// Element-Wise Functions

#define DEFINE_1D_HEADER(name)						\
  void fp32_##name(const struct ViewInstruction*, single_float*);	\
  void fp16_##name(const struct ViewInstruction*, fp16_t*);		\
  void fp8_##name(const struct ViewInstruction*, fp8_t*);		\
  void int_##name(const struct ViewInstruction*, int*);			\

DEFINE_1D_HEADER(abs);

DEFINE_1D_HEADER(sin);
DEFINE_1D_HEADER(cos);
DEFINE_1D_HEADER(tan);

DEFINE_1D_HEADER(asin);
DEFINE_1D_HEADER(acos);
DEFINE_1D_HEADER(atan);

DEFINE_1D_HEADER(sinh);
DEFINE_1D_HEADER(cosh);
DEFINE_1D_HEADER(tanh);

DEFINE_1D_HEADER(asinh);
DEFINE_1D_HEADER(acosh)
DEFINE_1D_HEADER(atanh);

DEFINE_1D_HEADER(log);
DEFINE_1D_HEADER(log2);
DEFINE_1D_HEADER(log10);

DEFINE_1D_HEADER(exp);
DEFINE_1D_HEADER(sqrt);
DEFINE_1D_HEADER(cbrt);

void fp32_scalar_greater_than(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*, single_float);

void fp32_scalar_less_than(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*, single_float);

void fp32_scalar_greater_than_eq(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*, single_float);

void fp32_scalar_less_than_eq(const struct ViewInstruction*, const struct ViewInstruction*, single_float*, single_float*, single_float);

