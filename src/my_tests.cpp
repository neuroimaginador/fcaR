// my_tests.cpp
// Internal development test stubs. Not exported to users.
// All [[Rcpp::export]] decorators removed intentionally.
#include <Rcpp.h>
using namespace Rcpp;
#include "set_operations_galois.h"

void test_new(S4 A) {
  SparseVector V = S4toSparse(A);
  freeVector(&V);
}

S4 test_export_new(S4 A) {
  SparseVector V = S4toSparse(A);
  S4 res = SparseToS4_fast(V);
  freeVector(&V);
  return res;
}

S4 test_extent_new(S4 A, NumericMatrix I) {
  return compute_extent(A, I, "standard", "Godel");
}

S4 test_intent_new(S4 A, NumericMatrix I) {
  return compute_intent(A, I, "standard", "Godel");
}
