#include <Rcpp.h>
using namespace Rcpp;

#include <R.h>
#include <Rdefines.h>

// [[Rcpp::export]]
NumericVector flatten_sparse_C(IntegerVector p,
                               IntegerVector i,
                               NumericVector x,
                               NumericVector dims) {

  int num_rows = dims[0];
  int num_cols = dims[1];

  NumericVector v(num_rows);

  for (int x_index = 0; x_index < num_cols; x_index++) {

    int start_index = p[x_index], end_index = p[x_index + 1];

    for (int j = start_index; j < end_index; j++) {

      if (x[j] > v[i[j]]) {

        v[i[j]] = x[j];

      }

    }

  }

  return v;

}


