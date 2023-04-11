
#include "xMatrix.h"

#include <immintrin.h>
#include <x86intrin.h>

#include <stdio.h>
#include <stdint.h>


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


int main() {
  printf("%d", cpu_has_avx2());
  return 1;
}
