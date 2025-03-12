#include <Rcpp.h>
using namespace Rcpp;
#include "set_operations_galois.h"

// [[Rcpp::export]]
void test_new(S4 A) {

  SparseVector V = S4toSparse(A);

}

// [[Rcpp::export]]
S4 test_export_new(S4 A) {

  SparseVector V = S4toSparse(A);
  S4 res = SparseToS4_fast(V);
  freeVector(&V);

  return res;

}

// [[Rcpp::export]]
S4 test_extent_new(S4 A, NumericMatrix I) {

  // Rcout << "Aquí" << std::endl;

  return compute_extent(A, I,
                        "standard",
                        "Zadeh");

}

// [[Rcpp::export]]
S4 test_intent_new(S4 A, NumericMatrix I) {

  // Rcout << "Aquí" << std::endl;

  return compute_intent(A, I,
                        "standard",
                        "Zadeh");

}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
library(fcaR)
I <- planets
S <- Set$new(attributes = colnames(planets))
S$assign(near = 1)
A <- S$get_vector()
# for (i in seq(100)) test_export_new(A)
B <- test_extent_new(A, I)
B
test_intent_new(B, I)
*/
