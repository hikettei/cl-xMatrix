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
// 四回LOADできないっけ？

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
	int index = view.offset + mi * view.stride2 + ni * view.stride1; \
	(simd_operation);						\
      }									\
      for(int ni=last_ni_index_for_rem;ni<view.n;ni++) {		\
	int index = view.offset + mi * view.stride2 + ni * view.stride1; \
	(reminder);							\
      }									\
    }									\
  } while(0)


static inline void fp32_abs_simd(single_float* x, __m256 sign_mask, int i) {
  XMAT_FP32_LOADU_PS(vec, &x[i]);
  __m256 abs_vec = _mm256_andnot_ps(sign_mask, vec);
  _mm256_storeu_ps(&x[i], abs_vec);
}

static inline void fp32_abs_scalarwise(single_float* x, int i) {
  if (x[i] < 0) {
    x[i] = -x[i];
  }
}

void fp32_abs(const struct ViewInstruction view, single_float* vec) {
  __m256 sign_mask = _mm256_set1_ps(-0.0f);
  WITH_VIEW_ITER(view, i, FP32_SIMD_STEP,
		 fp32_abs_simd(vec, sign_mask, i),
		 fp32_abs_scalarwise(vec, i));
}

/*
static inline void fp16_abs_simd(fp16_t* x, __m256 sign_mask, int i) {
  __m256 vec = _mm256_loadu_ps(&x[i]);
  __m256 abs_vec = _mm256_andnot_ps(sign_mask, vec);
  _mm256_storeu_ps(&x[i], abs_vec);
}

static inline void fp16_abs_scalarwise(fp16_t* x, __m256 sign_mask, int i) {
  if (x[i] < 0) {
    x[i] = -x[i];
  }
}


void fp16_abs(const struct ViewInstruction view, single_float* vec) {
  __m256 sign_mask = _mm256_set1_ps(-0.0f);
  WITH_VIEW_ITER(view, i, 2 * FP32_SIMD_STEP,
		 fp16_abs_simd(vec, sign_mask, i),
		 fp16_abs_scalarwise(vec, i));
}


*/
