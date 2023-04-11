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

#define WITH_VIEW_ITER(view, index, stride, element_wise_operation)	\
  do {									\
    for (int mi = view.offset2; mi < view.m; mi+=stride) {			\
      for (int ni = view.offset1; ni < view.n; ni+=stride) {			\
        int index = view.offset + mi * view.stride2 + ni * view.stride1; \
	(element_wise_operation);					\
      }									\
    }									\
  } while(0)

static inline single_float fp32_scalar_abs(single_float x) {
  // k~k+16をSIMDで並列化する
  if (x > 0) {
    return x;
  } else {
    return -1.0f * x;
  }
}

void fp32_abs(const struct ViewInstruction view, single_float* vec) {
  WITH_VIEW_ITER(view, k, 16, fp32_scalar_abs(vec[k]));
}
