// #include <xMatrix.h>

#include <immintrin.h>
#include <stdio.h>
#include <stdint.h>

// uint4_axpy (int offset, uint4_t* a, uint4_t* b, uint4_t* c)
int uint8_t_axpy(int offset_a,
		 int offset_b,
		 int offset_c,
		 uint8_t* a,
		 uint8_t* b,
		 uint8_t* c,
		 int m,
		 int n,
		 int stride_m,
		 int stride_n) {
  /*  [[0 1 2]
      [3 4 5]
      [6 7 8]]
      [[0 1 2]
      [3 4 5]
      [6 7 8]]
      => [0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9]
  */
  for (int mi=0;mi<m;mi+=stride_m) {
    for (int ni=0;ni<n;ni+=stride_n) {
      c[offset_c + mi * stride_m + ni * stride_n] = a[offset_c + mi * stride_m + ni * stride_n] + b[offset_c + mi * stride_m + ni * stride_n];
    }
  }
  return 0;
}

void simdDemo_step(void)
{
  int32_T i;
  for (i = 0; i <= 236; i += 4) {
    _mm_storeu_ps(&simdDemo_Y.Out1[i], _mm_sub_ps(_mm_loadu_ps(&simdDemo_U.In1[i]),
      _mm_loadu_ps(&simdDemo_U.In2[i])));
  }

  for (i = 0; i <= 138; i += 2) {
    _mm_storeu_pd(&simdDemo_Y.Out2[i], _mm_div_pd(_mm_loadu_pd(&simdDemo_U.In3[i]),
      _mm_loadu_pd(&simdDemo_U.In4[i])));
  }
}

int main() {
  return 1;
}
