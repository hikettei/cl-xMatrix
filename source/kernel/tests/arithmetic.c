#include <xMatrix.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

single_float* init_single_float_matrix(int m, int n) {
  single_float* result = (single_float*)malloc(m * n * sizeof(single_float));

  for (int mi=0;mi<m;mi++) {
    for (int ni=0;ni<n;ni++) {
      result[mi * m + ni] = 10.0f;
    }
  }
  return result;
}

void print_matrix(int m, int n, single_float* a) {
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      printf("matrix[%d][%d] = %f\n", i, j, a[i * m + j]);
    }
  }
}


int main () {
  struct ViewInstruction view = {
    .offset=0,
    .stride2=10,
    .stride1=1,
    .offset2=0,
    .offset1=0,
    .m=10,
    .n=10
  };
  
  single_float* a = init_single_float_matrix(10, 10);
  single_float* b = init_single_float_matrix(10, 10);

  fp32_abs(view, a);

  print_matrix(10, 10, a);

  return 0;
}
