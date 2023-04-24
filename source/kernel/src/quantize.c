#include "xMatrix.h"
#include "quantize.h"

#include <math.h>
#include <stdint.h>
#include <stdlib.h>

// FP32 <-> FP16

// ref: https://github.com/Maratyszcza/FP16 and https://github.com/ggerganov/ggml/blob/553929cf771634fc29b0700967a0621d56647f09/src/ggml.c#L217

static inline float fp32_from_bits(uint32_t w) {
    union {
        uint32_t as_bits;
        float as_value;
    } fp32;
    fp32.as_bits = w;
    return fp32.as_value;
}

static inline uint32_t fp32_to_bits(float f) {
	union {
		float as_value;
		uint32_t as_bits;
	} fp32;
	fp32.as_value = f;
	return fp32.as_bits;
}


static inline float compute_fp16_to_fp32(fp16_t h) {
    const uint32_t w = (uint32_t) h << 16;
    const uint32_t sign = w & UINT32_C(0x80000000);
    const uint32_t two_w = w + w;

    const uint32_t exp_offset = UINT32_C(0xE0) << 23;
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)
    const float exp_scale = 0x1.0p-112f;
#else
    const float exp_scale = fp32_from_bits(UINT32_C(0x7800000));
#endif
    const float normalized_value = fp32_from_bits((two_w >> 4) + exp_offset) * exp_scale;

    const uint32_t magic_mask = UINT32_C(126) << 23;
    const float magic_bias = 0.5f;
    const float denormalized_value = fp32_from_bits((two_w >> 17) | magic_mask) - magic_bias;

    const uint32_t denormalized_cutoff = UINT32_C(1) << 27;
    const uint32_t result = sign |
        (two_w < denormalized_cutoff ? fp32_to_bits(denormalized_value) : fp32_to_bits(normalized_value));
    return fp32_from_bits(result);
}


static inline fp16_t compute_fp32_to_fp16(float f) {
#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) || defined(__GNUC__) && !defined(__STRICT_ANSI__)
    const float scale_to_inf = 0x1.0p+112f;
    const float scale_to_zero = 0x1.0p-110f;
#else
    const float scale_to_inf = fp32_from_bits(UINT32_C(0x77800000));
    const float scale_to_zero = fp32_from_bits(UINT32_C(0x08800000));
#endif
    float base = (fabsf(f) * scale_to_inf) * scale_to_zero;

    const uint32_t w = fp32_to_bits(f);
    const uint32_t shl1_w = w + w;
    const uint32_t sign = w & UINT32_C(0x80000000);
    uint32_t bias = shl1_w & UINT32_C(0xFF000000);
    if (bias < UINT32_C(0x71000000)) {
        bias = UINT32_C(0x71000000);
    }

    base = fp32_from_bits((bias >> 1) + UINT32_C(0x07800000)) + base;
    const uint32_t bits = fp32_to_bits(base);
    const uint32_t exp_bits = (bits >> 13) & UINT32_C(0x00007C00);
    const uint32_t mantissa_bits = bits & UINT32_C(0x00000FFF);
    const uint32_t nonsign = exp_bits + mantissa_bits;
    return (sign >> 16) | (shl1_w > UINT32_C(0xFF000000) ? UINT16_C(0x7E00) : nonsign);
}


#define WITH_ELWISE_VIEW(view, index, element_wise_operation)		\
  do {									\
    for (int mi = 0; mi < view.m; mi++) {				\
      int tmp = view.offset + mi * view.stride2 * view.broadcast2;	\
      if (view.broadcast1 == 1) {					\
	for (int ni = 0; ni < view.n; ni++) {				\
	  int index = tmp + ni;						\
	  (element_wise_operation);					\
	}								\
      } else {								\
	for (int ni = 0; ni < view.n; ni++) {				\
	  int index = tmp;						\
	  (element_wise_operation);					\
	}								\
      }									\
    }									\
  } while(0)								\


fp16_t* convert_fp32_into_fp16_within_view(const struct ViewInstruction view, single_float* x) {
  fp16_t* result = (fp16_t*)malloc(view.m * view.n * sizeof(fp16_t));

  WITH_ELWISE_VIEW(view, i, result[i] = compute_fp32_to_fp16(x[i]));
  
  free(x);
  return result;
}


single_float* convert_fp16_into_fp32_within_view(const struct ViewInstruction view, fp16_t* x) {
  single_float* result = (single_float*)malloc(view.m * view.n * sizeof(single_float));

  WITH_ELWISE_VIEW(view, i, result[i] = compute_fp16_to_fp32(x[i]));
  
  free(x);
  return result;
}


fp16_t* convert_fp32_into_fp16(int size, single_float* x) {
  fp16_t* result = (fp16_t*)malloc(size * sizeof(fp16_t));

  for (int i=0; i<size; i++) {
    result[i] = compute_fp32_to_fp16(x[i]);
  }
  
  free(x);
  return result;
}


single_float* convert_fp16_into_fp32(int size, fp16_t* x) {
  single_float* result = (single_float*)malloc(size * sizeof(single_float));

  for (int i=0; i<size; i++) {
    result[i] = compute_fp16_to_fp32(x[i]);
  }
  
  free(x);
  return result;
}


// Todo:
// Ref (Including To read):
// https://papers.nips.cc/paper/2020/file/13b919438259814cd5be8cb45877d577-Paper.pdf
// https://arxiv.org/pdf/1810.05723.pdf
// https://arxiv.org/pdf/2301.12017v1.pdf
// FP32 <-> INT4
