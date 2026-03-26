#include <Rcpp.h>
#include "fcaR_bitset.h"

using namespace Rcpp;
using namespace fcaR;

//' @title compute_arrow_relations_cpp
//' @description Computes the arrow relations (swarrow, nearrow, and double arrow)
//' for a binary formal context.
//' @param I (IntegerMatrix) The binary incidence matrix of the formal context.
//' @return An IntegerMatrix where:
//'   - 1: \swarrow
//'   - 2: \nearrow
//'   - 3: \updownarrow
//' @noRd
// [[Rcpp::export]]
IntegerMatrix compute_arrow_relations_cpp(IntegerMatrix I) {
  int n_obj = I.nrow();
  int n_attr = I.ncol();

  std::vector<FastBitset> rows(n_obj);
  std::vector<FastBitset> cols(n_attr);

  for (int i = 0; i < n_obj; ++i) {
    rows[i].init(n_attr);
    for (int j = 0; j < n_attr; ++j) {
      if (I(i, j) != 0) rows[i].set(j);
    }
  }

  for (int j = 0; j < n_attr; ++j) {
    cols[j].init(n_obj);
    for (int i = 0; i < n_obj; ++i) {
      if (I(i, j) != 0) cols[j].set(i);
    }
  }

  IntegerMatrix res(n_obj, n_attr);

  // Compute swarrow (down-left)
  for (int i = 0; i < n_obj; ++i) {
    // H_i = {h | rows[i] \subsetneq rows[h]}
    FastBitset intersection;
    intersection.init(n_attr);
    intersection.set(); // Start with all attributes

    bool h_empty = true;
    for (int h = 0; h < n_obj; ++h) {
      if (h == i) continue;
      // rows[i] \subsetneq rows[h]
      if (rows[i].is_subset_of(rows[h]) && rows[i] != rows[h]) {
        intersection &= rows[h];
        h_empty = false;
      }
    }

    for (int j = 0; j < n_attr; ++j) {
      if (I(i, j) == 0) {
        // (i, j) \in \swarrow iff j \in intersection
        if (h_empty || intersection.test(j)) {
          res(i, j) |= 1;
        }
      }
    }
  }

  // Compute nearrow (up-right)
  for (int j = 0; j < n_attr; ++j) {
    // N_j = {n | cols[j] \subsetneq cols[n]}
    FastBitset intersection;
    intersection.init(n_obj);
    intersection.set();

    bool n_empty = true;
    for (int n = 0; n < n_attr; ++n) {
      if (n == j) continue;
      if (cols[j].is_subset_of(cols[n]) && cols[j] != cols[n]) {
        intersection &= cols[n];
        n_empty = false;
      }
    }

    for (int i = 0; i < n_obj; ++i) {
      if (I(i, j) == 0) {
        if (n_empty || intersection.test(i)) {
          res(i, j) |= 2;
        }
      }
    }
  }

  return res;
}
