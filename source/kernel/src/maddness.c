
#include "maddness.h"

// Multiplying Matrices Without Multiplying
// https://arxiv.org/pdf/2106.10860.pdf

// Maddness Approximates this formula:
// A.T * B (where A = (N, D), B = (D, M))
// Maddness requires N >> (M, D) (i.e.: The assumption is that: the longer MHA's source sentence, the better work Maddness does)

// However my library is designed to be used in FP16, so here's some minor changes:
//
//

// A/T @ B ~= A*.T @ B where A* is a training data(called offline data)
// training_offline obtains A* but the given a is type of FP16

fp16_t* training_offline(const fp16_t* a, int K, int C) {
  // This function includes: Prototype learning, Encoding, Table Construction
  // a is usually model's weight or MHA's source sentence (not known to be practical)
  // Let K be a number of prototypes per subspace, C be a number of subspaces.

  
  
 
  
  
}



void aggregation () {

}
