#pragma SIMD
#pragma GCC optimize ("O3")
#pragma GCC target ("avx2")

#include "maddness.h"

#include <immintrin.h>

#include <math.h>
#include <stdio.h>
#include <stdint.h>
#include <assert.h>

// FP32
// Maddness Encode
// dense_lut <- create lut

static inline float min(float x, float y) { return x < y ? x : y; }
static inline float max(float x, float y) { return x > y ? x : y; }


void mithral_encode(const float *X, int64_t nrows, int ncols,
                    const uint32_t *splitdims, const int8_t *all_splitvals,
                    const float *scales, const float *offsets, int ncodebooks,
                    uint8_t *out)
// const float* scales, int ncodebooks, uint8_t* out)
{
  // ncodebooks = K
  static constexpr bool DeferPerm = true;
  static constexpr int block_nrows = 32;
  static constexpr int nsplits_per_codebook = 4;
  static constexpr int vals_per_split = 1 << nsplits_per_codebook; // 16
  const int64_t nblocks = ceil(nrows / (double)block_nrows);
  assert(nrows % block_nrows == 0); // TODO remove this constraint

  // sanity check splits
  auto total_nsplits = ncodebooks * nsplits_per_codebook;
  auto maxdim = splitdims[0];
  auto mindim = splitdims[0];
  for (int i = 1; i < total_nsplits; i++) {
    maxdim = MAX(maxdim, splitdims[i]);
    mindim = MIN(maxdim, splitdims[i]);
  }
  assert(mindim >= 0);
  assert(maxdim < ncols);

  size_t x_col_stride = nrows;
  size_t out_col_stride = nrows;
  const float *x_ptrs[nsplits_per_codebook];
  __m256i current_vsplitval_luts[nsplits_per_codebook];
  __m256 current_vscales[nsplits_per_codebook];
  __m256 current_voffsets[nsplits_per_codebook];

  int split_idx = 0;
  for (int c = 0; c < ncodebooks; c++) {
    // compute input and output column starts
    auto out_ptr = out + (out_col_stride * c);
    for (int s = 0; s < nsplits_per_codebook; s++) {
      auto splitdim = splitdims[split_idx + s];
      x_ptrs[s] = X + (x_col_stride * splitdim);
      auto splitvals_ptr = all_splitvals + (vals_per_split * split_idx);
      current_vsplitval_luts[s] = _mm256_broadcastsi128_si256(
          load_si128i((const __m128i *)splitvals_ptr));
      current_vscales[s] = _mm256_set1_ps(scales[split_idx + s]);
      current_voffsets[s] = _mm256_set1_ps(offsets[split_idx + s]);
    }
    split_idx += nsplits_per_codebook;

    for (int b = 0; b < nblocks; b++) { // for each block
      __m256i codes = _mm256_setzero_si256();
#pragma unroll
      for (int s = 0; s < nsplits_per_codebook; s++) {
        auto vscales = current_vscales[s];
        auto voffsets = current_voffsets[s];
        // auto voffsets = _mm256_setzero_si256();
        auto vsplitvals_lut = current_vsplitval_luts[s];
        auto vsplitvals =  _mm256_shuffle_epi8(vsplitvals_lut, codes); // codes = group_ids

        auto x_ptr = x_ptrs[s];
        x_ptrs[s] += block_nrows;

        // true = signed saturation; better because cmp instructions
        // exist for epi8 but not epu8
        auto x_i8 = load_4xf32_as_32xepi8_or_epu8<true, !DeferPerm>(
            // x_ptr, vscales);
            x_ptr, vscales, voffsets);

        auto masks = _mm256_cmpgt_epi8(x_i8, vsplitvals);
        // map -1 -> 1; 0 stays the same
        auto masks_0_or_1 = _mm256_sign_epi8(masks, masks);

        if (s > 0) {
          // shift left by multiplying by 2, by adding to itself
          codes = _mm256_add_epi8(codes, codes);
        }

        // OR in new low bit
        codes = _mm256_or_si256(codes, masks_0_or_1);
      }
      if (DeferPerm) {
        codes = _mm256_permutevar8x32_epi32(
            codes, _mm256_setr_epi32(0, 4, 1, 5, 2, 6, 3, 7));
      }
      _mm256_storeu_si256((__m256i *)out_ptr, codes);
      out_ptr += block_nrows;
    }
  }
}


void fp32_maddness_encode(const float *X,
			  int m,
			  int n,
			  const int *splitdims,
			  const float *scales,
			  const float *offsets,
			  int ncodebooks, // The size of prototypes.
			  int nsplits, // The depth of trees
			  uint8_t *out) {
  /* split-dims, scales, offsets are given in this format:
     [@Bucket(0).scales, @Bucket(1).scales, @Bucket(2).scales ... @Bucket(4).scales] + f(Prototype-1) + f(Prototype-2) + ...

     *X   ... [m, n].T
     *out ... [m, ncodebooks] where each element is the type of (the integer 0 2^4)
     Note: *X is colum-major order (transpose it in advance)?
     C0 ... Cn is continuous.
  */

  int K = 2^nsplits;
  int nsplits_per_codebook = 1 + 2 + 3 + 4;

  /* First for iteration: Compute by prototypes.
     D
    M++-
    -++
  N -++-
    -++- C=2,
    -++- The width of M = coffest.
    out

    ncodebooks
    ---
    ---
 N  +++ ↓ Iterate in this order.
    ---
    ---
    
   */

  size_t stride     = m;
  size_t out_stride = ncodebooks;

  // Tmp [Proto(1), Proto(2), Proto(3), ...]
  const float *x_ptrs[nsplits];
  __m256i current_vsplitval_luts[nsplits];
  __m256 current_vscales[nsplits];
  __m256 current_voffsets[nsplits];
  
  int split_idx = 0; // whichnth splits? (offset)
  for (int c=0;c<ncodebooks;c++) {
    uint8_t* out_ptr = out + (out_stride * c);
    
    for (int s = 0; s < nsplits; s++) {
      float splitdim = splitdims[split_idx + s];
      x_ptrs[s] = X + (x_col_stride * splitdim);
      
      auto splitvals_ptr = all_splitvals + (vals_per_split * split_idx);
      current_vsplitval_luts[s] = _mm256_broadcastsi128_si256(load_si128i((const __m128i *)splitvals_ptr));
      current_vscales[s] = _mm256_set1_ps(scales[split_idx + s]);
      current_voffsets[s] = _mm256_set1_ps(offsets[split_idx + s]);
    }    
  }  
}
