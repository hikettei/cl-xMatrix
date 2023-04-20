#pragma SIMD
#include "xMatrix.h"

#include <immintrin.h>
/*
+----------------+------------------------------------------------------------------------------------------+
|     Header     |                                         Purpose                                          |
+----------------+------------------------------------------------------------------------------------------+
| x86intrin.h    | Everything, including non-vector x86 instructions like _rdtsc().                         |
| mmintrin.h     | MMX (Pentium MMX!)                                                                       |
| mm3dnow.h      | 3dnow! (K6-2) (deprecated)                                                               |
| xmmintrin.h    | SSE + MMX (Pentium 3, Athlon XP)                                                         |
| emmintrin.h    | SSE2 + SSE + MMX (Pentium 4, Athlon 64)                                                  |
| pmmintrin.h    | SSE3 + SSE2 + SSE + MMX (Pentium 4 Prescott, Athlon 64 San Diego)                        |
| tmmintrin.h    | SSSE3 + SSE3 + SSE2 + SSE + MMX (Core 2, Bulldozer)                                      |
| popcntintrin.h | POPCNT (Nehalem (Core i7), Phenom)                                                       |
| ammintrin.h    | SSE4A + SSE3 + SSE2 + SSE + MMX (AMD-only, starting with Phenom)                         |
| smmintrin.h    | SSE4_1 + SSSE3 + SSE3 + SSE2 + SSE + MMX (Penryn, Bulldozer)                             |
| nmmintrin.h    | SSE4_2 + SSE4_1 + SSSE3 + SSE3 + SSE2 + SSE + MMX (Nehalem (aka Core i7), Bulldozer)     |
| wmmintrin.h    | AES (Core i7 Westmere, Bulldozer)                                                        |
| immintrin.h    | AVX, AVX2, AVX512, all SSE+MMX (except SSE4A and XOP), popcnt, BMI/BMI2, FMA             |
+----------------+------------------------------------------------------------------------------------------+
 */

#include <stdio.h>
#include <stdint.h>

// cpu identifications

int cpu_has_sse(void){
#if defined(__SSE__)
    return 1;
#else
    return 0;
#endif
}

int cpu_has_avx(void){
#if defined(__AVX__)
    return 1;
#else
    return 0;
#endif
}

int cpu_has_avx2(void){
#if defined(__AVX2__)
    return 1;
#else
    return 0;
#endif
}

int cpu_has_avx512(void){
#if defined(__AVX512__)
    return 1;
#else
    return 0;
#endif
}

// Configs about SIMD

#define ALIGN 64

#if defined(__AVX2__)

#define FP32_SIMD_STEP 8
#define XMAT_FP32_LOADU_PS(var, pointer) __m256 var = _mm256_loadu_ps(pointer);

#elif defined(__AVX__)

#define FP32_SIMD_STEP 8

#endif

// Macros for STORING AND LOADING

// simd_operation -> SIMD Operation, reminder = element_wise_operation
#define WITH_VIEW_ITER(view, index, stride, simd_operation, reminder)	\
  do {									\
    int last_ni_index_for_rem = 0;					\
    for (int mi = view.offset2; mi < view.m; mi++) {    		\
      for (int ni = view.offset1; ni < view.n; ni+=stride) {		\
	last_ni_index_for_rem = ni+stride;				\
	int index = view.offset + mi * view.stride2 * view.broadcast2 + ni * view.stride1 * view.broadcast1; \
	(simd_operation);						\
      }									\
      for(int ni=last_ni_index_for_rem;ni<view.n;ni++) {		\
	int index = view.offset + mi * view.stride2 * view.broadcast2 + ni * view.stride1 * view.broadcast1; \
	(reminder);							\
      }									\
    }									\
  } while(0)


// A + B
#define WITH_VIEW_OPS(view1, view2, index1, index2, stride, simd_operation, reminder) \
  do {									\
    int last_ni_index_for_rem = 0;					\
    for (int mi = view1.offset2; mi < view1.m; mi++) {    		\
      for (int ni = view1.offset1; ni < view1.n; ni+=stride) {		\
	last_ni_index_for_rem = ni+stride;				\
	int index1 = view1.offset + mi * view1.stride2 * view1.broadcast2 + ni * view1.stride1 * view1.broadcast1; \
	int index2 = view2.offset + mi * view2.stride2 * view2.broadcast2 + ni * view2.stride1 * view2.broadcast1; \
	(simd_operation);						\
      }									\
      for(int ni=last_ni_index_for_rem;ni<view1.n;ni++) {		\
	int index1 = view1.offset + mi * view1.stride2 * view1.broadcast2 + ni * view1.stride1 * view1.broadcast1; \
	int index2 view2.offset + mi * view2.stride2 * view2.broadcast2 + ni * view2.stride1 * view2.broadcast1; \
	(reminder);							\
      }									\
    }									\
  } while(0)


static inline void fp32_abs_simd(single_float* x, __m256 sign_mask, int i) {
  XMAT_FP32_LOADU_PS(vec, &x[i]);
  __m256 abs_vec = _mm256_andnot_ps(sign_mask, vec);
  _mm256_storeu_ps(&x[i], abs_vec);
}

#define DEFINE_ABS_ELEMENTWISE(name, dtype)	\
  static inline void name(dtype* x, int i) {	\
    if (x[i] < 0) {				\
      x[i] = -x[i];				\
    }						\
  }						\
    
DEFINE_ABS_ELEMENTWISE(fp32_abs_scalarwise, single_float);
DEFINE_ABS_ELEMENTWISE(fp16_abs_scalarwise, fp16_t);
DEFINE_ABS_ELEMENTWISE(fp8_abs_scalarwise,  fp8_t);
DEFINE_ABS_ELEMENTWISE(int_abs_scalarwise,  int);

// To Add: RELU, SIGMOID, TANH etc...
void fp32_abs(const struct ViewInstruction view, single_float* vec) {
  __m256 sign_mask = _mm256_set1_ps(-0.0f);
  WITH_VIEW_ITER(view, i, FP32_SIMD_STEP,
		 fp32_abs_simd(vec, sign_mask, i),
		 fp32_abs_scalarwise(vec, i));
}

#define WITH_ELWISE_VIEW(view, index, element_wise_operation)		\
  do {									\
    for (int mi = view.offset2; mi < view.m; mi++) {			\
      for (int ni = view.offset1; ni < view.n; ni++) {			\
        int index = view.offset + mi * view.stride2 * view.broadcast2 + ni * view.stride1 * view.broadcast1; \
	(element_wise_operation);					\
      }									\
    }									\
  } while(0)

#define WITH_ELWISE_OPS(view1, view2, index1, index2, element_wise_operation) \
  do {									\
    for (int mi = view1.offset2; mi < view1.m; mi++) {			\
      for (int ni = view1.offset1; ni < view1.n; ni++) {		\
        int index1 = view1.offset + mi * view1.stride2 * view1.broadcast2 + ni * view1.stride1 * view1.broadcast1; \
	int index2 = view2.offset + mi * view2.stride2 * view2.broadcast2 + ni * view2.stride1 * view2.broadcast1; \
	(element_wise_operation);					\
      }									\
    }									\
  } while(0)

// TODO: Current Operations are temporary, make them SIMD.
void fp32_copy(const struct ViewInstruction view, const struct ViewInstruction view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void fp16_copy(const struct ViewInstruction view, const struct ViewInstruction view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void fp8_copy(const struct ViewInstruction view, const struct ViewInstruction view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void int_copy(const struct ViewInstruction view, const struct ViewInstruction view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}


// TODO: SIMD
void fp32_add(const struct ViewInstruction view, const struct ViewInstruction view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void fp16_add(const struct ViewInstruction view, const struct ViewInstruction view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void fp8_add(const struct ViewInstruction view, const struct ViewInstruction view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void int_add(const struct ViewInstruction view, const struct ViewInstruction view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}


void fp32_sub(const struct ViewInstruction view, const struct ViewInstruction view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void fp16_sub(const struct ViewInstruction view, const struct ViewInstruction view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void fp8_sub(const struct ViewInstruction view, const struct ViewInstruction view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void int_sub(const struct ViewInstruction view, const struct ViewInstruction view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}


void fp32_mul(const struct ViewInstruction view, const struct ViewInstruction view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void fp16_mul(const struct ViewInstruction view, const struct ViewInstruction view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void fp8_mul(const struct ViewInstruction view, const struct ViewInstruction view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void int_mul(const struct ViewInstruction view, const struct ViewInstruction view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}


// I guess they're dangerous
void fp32_div(const struct ViewInstruction view, const struct ViewInstruction view1, single_float* vec1, single_float* vec2) {
  // Vec2 has no any offsets. So i must be absolute
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void fp16_div(const struct ViewInstruction view, const struct ViewInstruction view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void fp8_div(const struct ViewInstruction view, const struct ViewInstruction view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void int_div(const struct ViewInstruction view, const struct ViewInstruction view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}




void fp32_scalar_add(const struct ViewInstruction view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void fp16_scalar_add(const struct ViewInstruction view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void fp8_scalar_add(const struct ViewInstruction view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void int_scalar_add(const struct ViewInstruction view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}


void fp32_scalar_mul(const struct ViewInstruction view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void fp16_scalar_mul(const struct ViewInstruction view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void fp8_scalar_mul(const struct ViewInstruction view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void int_scalar_mul(const struct ViewInstruction view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}


void fp32_fill(const struct ViewInstruction view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void fp16_fill(const struct ViewInstruction view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void fp8_fill(const struct ViewInstruction view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void int_fill(const struct ViewInstruction view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}



