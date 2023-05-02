#pragma SIMD
#pragma GCC optimize ("O3")
#pragma GCC target ("avx2")

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

#include <math.h>
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


// aligned_malloc is defined in C11, while MSVC doesn't.
#if defined(_aligned_alloc)
#define aligned_alloc _aligned_alloc
#endif

#if defined(__AVX2__)

#define FP32_SIMD_STEP 8   // float32
#define FP16_SIMD_STEP 16  // uint16_t
#define FP8_SIMD_STEP 32   // uint8_t

// (Note for myself) Dtypes:
// (AVX, AVX2 2008 onwards)
// __mm256  <- float32 * 8
// __mm256i <- 4*uint64, 8*uint32, 16*uint16, 32*uint8
// __mm256d <- 4*float64

// _ps packed single_floats, _pd packed double_floats
// epin packed intn
// epun packed unsigned_intn

#define XMAT_FP32_LOADU(var, pointer) __m256 var = _mm256_loadu_ps(pointer);
#define XMAT_FP16_LOADU(var, pointer) __m256i var = _mm256_loadu_si256((__m256i*)pointer);
#define XMAT_FP8_LOADU(var, pointer) __m256i var = _mm256_loadu_si256((__m256i*)pointer);

#define XMAT_FP32_STOREU(target, pointer) _mm256_storeu_ps(target, pointer);
#define XMAT_FP16_STOREU(target, pointer) _mm256_storeu_si256((__m256i*)target, pointer);
#define XMAT_FP8_STOREU(target, pointer)  _mm256_storeu_si256((__m256i*)target, pointer);

#elif defined(__AVX__)

#define FP32_SIMD_STEP 8

#endif

// Problem: Memory Aligment with CFFI.
// Depcrecated
single_float* fp32_allocate_aligned_mat(int size) {
  return (single_float*)aligned_alloc(ALIGN, size * sizeof(single_float));
}

fp16_t* fp16_allocate_aligned_mat(int size) {
  return (fp16_t*)aligned_alloc(ALIGN, size * sizeof(fp16_t));
}

fp8_t* fp8_allocate_aligned_mat(int size) {
  return (fp8_t*)aligned_alloc(ALIGN, size * sizeof(fp8_t));
}

int* int_allocate_aligned_mat(int size) {
  return (int*)aligned_alloc(ALIGN, size * sizeof(int));
}

// Macros for STORING AND LOADING

// f(x)
// simd_operation -> SIMD Operation, reminder = element_wise_operation
#define WITH_VIEW_ITER(view, index, stride, simd_operation, reminder)	\
  do {									\
    int last_ni_index_for_rem = 0;					\
    for (int mi = view->offset2; mi < view->m; mi++) {    		\
      for (int ni = view->offset1; ni < view->n; ni+=stride) {		\
	last_ni_index_for_rem = ni+stride;				\
	int index = view->offset + mi * view->stride2 * view->broadcast2 + ni * view->stride1 * view->broadcast1; \
	(simd_operation);						\
      }									\
      for(int ni=last_ni_index_for_rem;ni<view->n;ni++) {		\
	int index = view->offset + mi * view->stride2 * view->broadcast2 + ni * view->stride1 * view->broadcast1; \
	(reminder);							\
      }									\
    }									\
  } while(0)


// f(a, b) but SIMD.
#define WITH_VIEW_OPS(view1, view2, index1, index2, stride, simd_operation, reminder) \
  do {									\
    int last_ni_index_for_rem = 0;					\
    for (int mi = 0; mi < view1->m; mi++) {				\
      int mi1 = mi + view1->offset2;					\
      int mi2 = mi + view2->offset2;					\
      for (int ni = 0; ni < view1->n; ni+=stride) {			\
	last_ni_index_for_rem = ni+stride;				\
	int ni1 = ni + view1->offset1;					\
	int ni2 = ni + view2->offset1;					\
	int index1 = view1->offset + mi1 * view1->stride2 * view1->broadcast2 + ni1 * view1->stride1 * view1->broadcast1; \
	int index2 = view2->offset + mi2 * view2->stride2 * view2->broadcast2 + ni2 * view2->stride1 * view2->broadcast1; \
	(simd_operation);						\
      }									\
      for(int ni=last_ni_index_for_rem;ni<view1->n;ni++) {		\
	int ni1 = ni + view1->offset1;					\
	int ni2 = ni + view2->offset1;					\
	int index1 = view1->offset + mi1 * view1->stride2 * view1->broadcast2 + ni1 * view1->stride1 * view1->broadcast1; \
	int index2 = view2->offset + mi2 * view2->stride2 * view2->broadcast2 + ni2 * view2->stride1 * view2->broadcast1; \
	(reminder);							\
      }									\
    }									\
  } while(0)


static inline void fp32_abs_simd(single_float* x, __m256 sign_mask, int i) {
  XMAT_FP32_LOADU(vec, &x[i]);
  __m256 abs_vec = _mm256_andnot_ps(sign_mask, vec);
  XMAT_FP32_STOREU(&x[i], abs_vec);
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
void fp32_abs(const struct ViewInstruction* view, single_float* vec) {
  __m256 sign_mask = _mm256_set1_ps(-0.0f);
  WITH_VIEW_ITER(view, i, FP32_SIMD_STEP,
		 fp32_abs_simd(vec, sign_mask, i),
  		 fp32_abs_scalarwise(vec, i));
}

// f(x)
#define WITH_ELWISE_VIEW(view, index, element_wise_operation)		\
  do {									\
    for (int mi = 0; mi < view->m; mi++) {				\
      int tmp = view->offset + mi * view->stride2 * view->broadcast2;	\
      if (view->broadcast1 == 1) {					\
	for (int ni = 0; ni < view->n; ni++) {				\
	  int index = tmp + ni;						\
	  (element_wise_operation);					\
	}								\
      } else {								\
	for (int ni = 0; ni < view->n; ni++) {				\
	  int index = tmp;						\
	  (element_wise_operation);					\
	}								\
      }									\
    }									\
  } while(0)								\


// f(x, y)
// Assertion: view1.m, view1.n = view2.m, view1.n
// Index = view.offset + (mi + view.offset2) * view.stride2 * view.broadcast
#define WITH_ELWISE_OPS(view1, view2, index1, index2, element_wise_operation) \
  do {									\
    int mi1_stride;							\
    int mi2_stride;							\
    int index1;								\
    int index2;								\
    int index1_maxlen;							\
    if (view1->broadcast2 == 0) { mi1_stride = 0; } else { mi1_stride = view1->stride2; } \
    if (view2->broadcast2 == 0) { mi2_stride = 0; } else { mi2_stride = view2->stride2; } \
    int mi1 = view1->offset2 * mi1_stride;				\
    int mi2 = view2->offset2 * mi2_stride;				\
    for (int m_index=0;m_index<view1->m;m_index++, mi1+=mi1_stride, mi2+=mi2_stride) { \
      if (view1->broadcast1 + view2->broadcast1 == 2) {			\
        index1_maxlen = view1->offset + view1->offset1 + mi1 + view1->n; \
	for (index1=view1->offset + view1->offset1 + mi1, index2 = view2->offset + view2->offset1 + mi2;index1 < index1_maxlen; index1++,index2++) { \
	  (element_wise_operation);					\
	}								\
      } else if (view1->broadcast1 + view2->broadcast1 == 0) {	        \
	index1 = view1->offset + mi1 + view1->offset1;			\
	index2 = view2->offset + mi2 + view2->offset1;			\
	for (int n_index=0;n_index<view1->n;n_index++) {		\
	  (element_wise_operation);					\
        }								\
      }	else if (view1->broadcast1 == 0) {				\
	index1 = view1->offset + view1->offset1 + mi1;			\
	index1_maxlen = view2->offset + view2->offset1 + mi2 + view2->n; \
	for (index2=view2->offset + view2->offset1 + mi2;index2<index1_maxlen;index2++) { \
	  (element_wise_operation);					\
	}								\
      }	else if (view2->broadcast1 == 0) {				\
	index2 = view2->offset + view2->offset1 + mi2;			\
	index1_maxlen = view1->offset + view1->offset1 + mi1 + view1->n; \
	for (index1=view1->offset + view1->offset1 + mi1;index1<index1_maxlen;index1++) { \
	  (element_wise_operation);					\
	}								\
      }									\
    }									\
  } while (0)


// TODO: Current Operations are temporary, make them SIMD.
void fp32_copy(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void fp16_copy(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void fp8_copy(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}

void int_copy(const struct ViewInstruction* view, const struct ViewInstruction* view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec2[m] = vec1[k]);
}




void fp32_add(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void fp16_add(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void fp8_add(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}

void int_add(const struct ViewInstruction* view, const struct ViewInstruction* view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] += vec2[m]);
}




void fp32_sub(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void fp16_sub(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void fp8_sub(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}

void int_sub(const struct ViewInstruction* view, const struct ViewInstruction* view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] -= vec2[m]);
}




void fp32_mul(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec1, single_float* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void fp16_mul(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void fp8_mul(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}

void int_mul(const struct ViewInstruction* view, const struct ViewInstruction* view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] *= vec2[m]);
}


// I guess they're dangerous
void fp32_div(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec1, single_float* vec2) {
  // Vec2 has no any offsets. So i must be absolute
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void fp16_div(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp16_t* vec1, fp16_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void fp8_div(const struct ViewInstruction* view, const struct ViewInstruction* view1, fp8_t* vec1, fp8_t* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}

void int_div(const struct ViewInstruction* view, const struct ViewInstruction* view1, int* vec1, int* vec2) {
  WITH_ELWISE_OPS(view, view1, k, m, vec1[k] /= vec2[m]);
}




void fp32_scalar_add(const struct ViewInstruction* view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void fp16_scalar_add(const struct ViewInstruction* view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void fp8_scalar_add(const struct ViewInstruction* view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}

void int_scalar_add(const struct ViewInstruction* view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] += scal);
}


void fp32_scalar_sub(const struct ViewInstruction* view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] -= scal);
}

void fp16_scalar_sub(const struct ViewInstruction* view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] -= scal);
}

void fp8_scalar_sub(const struct ViewInstruction* view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] -= scal);
}

void int_scalar_sub(const struct ViewInstruction* view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] -= scal);
}


void fp32_scalar_mul(const struct ViewInstruction* view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void fp16_scalar_mul(const struct ViewInstruction* view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void fp8_scalar_mul(const struct ViewInstruction* view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}

void int_scalar_mul(const struct ViewInstruction* view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] *= scal);
}


void fp32_scalar_div(const struct ViewInstruction* view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] /= scal);
}

void fp16_scalar_div(const struct ViewInstruction* view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] /= scal);
}

void fp8_scalar_div(const struct ViewInstruction* view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] /= scal);
}

void int_scalar_div(const struct ViewInstruction* view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] /= scal);
}


void fp32_fill(const struct ViewInstruction* view, single_float* vec1, single_float scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void fp16_fill(const struct ViewInstruction* view, fp16_t* vec1, fp16_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void fp8_fill(const struct ViewInstruction* view, fp8_t* vec1, fp8_t scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}

void int_fill(const struct ViewInstruction* view, int* vec1, int scal) {
  WITH_ELWISE_VIEW(view, k, vec1[k] = scal);
}




#define DEFINE_1D_FUNCTION(name, f)					\
  void fp32_##name(const struct ViewInstruction* view, single_float* vec) { \
    WITH_ELWISE_VIEW(view, k, vec[k]=f(vec[k]));			\
  }									\
  void fp16_##name(const struct ViewInstruction* view, fp16_t* vec) {	\
    WITH_ELWISE_VIEW(view, k, vec[k]=f(vec[k]));			\
  }									\
  void fp8_##name(const struct ViewInstruction* view, fp8_t* vec) {	\
    WITH_ELWISE_VIEW(view, k, vec[k]=f(vec[k]));			\
  }									\
  void int_##name(const struct ViewInstruction* view, int* vec) {	\
    WITH_ELWISE_VIEW(view, k, vec[k]=f(vec[k]));			\
  }									\

DEFINE_1D_FUNCTION(sin, sin);
DEFINE_1D_FUNCTION(cos, cos);
DEFINE_1D_FUNCTION(tan, tan);

DEFINE_1D_FUNCTION(asin, asin);
DEFINE_1D_FUNCTION(acos, acos);
DEFINE_1D_FUNCTION(atan, atan);

DEFINE_1D_FUNCTION(sinh, sinh);
DEFINE_1D_FUNCTION(cosh, cosh);
DEFINE_1D_FUNCTION(tanh, tanh);

DEFINE_1D_FUNCTION(asinh, asinh);
DEFINE_1D_FUNCTION(acosh, acosh);
DEFINE_1D_FUNCTION(atanh, atanh);

// log log2 log10 exp expt square sqrt pow


DEFINE_1D_FUNCTION(log, log);
DEFINE_1D_FUNCTION(log2, log2);
DEFINE_1D_FUNCTION(log10, log10);

DEFINE_1D_FUNCTION(exp, exp);
DEFINE_1D_FUNCTION(sqrt, sqrt);
DEFINE_1D_FUNCTION(cbrt, cbrt);

static inline void fp32_elm_scalar_greater_than(single_float* vec, single_float* out, single_float scalar, int k, int m) {
  if (vec[k] > scalar) {
    out[m] = 1.0;
  } else {
    out[m] = 0.0;
  }
}

static inline void fp32_elm_scalar_less_than(single_float* vec, single_float* out, single_float scalar, int k, int m) {
  if (vec[k] < scalar) {
    out[m] = 1.0;
  } else {
    out[m] = 0.0;
  }
}

static inline void fp32_elm_scalar_greater_than_eq(single_float* vec, single_float* out, single_float scalar, int k, int m) {
  if (vec[k] >= scalar) {
    out[m] = 1.0;
  } else {
    out[m] = 0.0;
  }
}

static inline void fp32_elm_scalar_less_than_eq(single_float* vec, single_float* out, single_float scalar, int k, int m) {
  if (vec[k] <= scalar) {
    out[m] = 1.0;
  } else {
    out[m] = 0.0;
  }
}

// out <- vec > scalar
void fp32_scalar_greater_than(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec, single_float* out, single_float scalar) {
  WITH_ELWISE_OPS(view, view1, k, m, fp32_elm_scalar_greater_than(vec, out, scalar, k, m));
}

void fp32_scalar_less_than(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec, single_float* out, single_float scalar) {
  WITH_ELWISE_OPS(view, view1, k, m, fp32_elm_scalar_less_than(vec, out, scalar, k, m));
}

void fp32_scalar_greater_than_eq(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec, single_float* out, single_float scalar) {
  WITH_ELWISE_OPS(view, view1, k, m, fp32_elm_scalar_greater_than_eq(vec, out, scalar, k, m));
}

void fp32_scalar_less_than_eq(const struct ViewInstruction* view, const struct ViewInstruction* view1, single_float* vec, single_float* out, single_float scalar) {
  WITH_ELWISE_OPS(view, view1, k, m, fp32_elm_scalar_less_than_eq(vec, out, scalar, k, m));
}

// Add:
// expt(base, power) pow(x, n)

// For disassemble
int main () {}
