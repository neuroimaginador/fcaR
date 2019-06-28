#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

double _I_(double x, double y) {

  if (x <= y) return(1);

  return(y);

}

double setdiffC(double x, double y) {

  if (y >= x) return(0);

  return(x);

}

// [[Rcpp::export]]
NumericVector intent_C(NumericVector A, NumericMatrix I) {

  NumericVector res(A.size());

  for (int c = 0; c < I.ncol(); c++) {

    double ms = 1;

    for (int r = 0; r < I.nrow(); r++) {

      double tmp = _I_(A[r], I(r, c));

      if (tmp < ms) ms = tmp;

    }

    res[c] = ms;

  }

  return(res);

}
// [[Rcpp::export]]
NumericVector extent_C(NumericVector A, NumericMatrix I) {

  NumericVector res(A.size());

  for (int r = 0; r < I.nrow(); r++) {

    double ms = 1;

    for (int c = 0; c < I.ncol(); c++) {

      double tmp = _I_(A[c], I(r, c));

      if (tmp < ms) ms = tmp;

    }

    res[r] = ms;

  }

  return(res);
}

// [[Rcpp::export]]
NumericVector closure_C(NumericVector A, NumericMatrix I) {

  return(intent_C(extent_C(A, I), I));

}


// [[Rcpp::export]]
List ganters_algorithm(NumericMatrix I,
                       NumericVector grades_set,
                       int n_attributes) {

  NumericVector empty(n_attributes);

  NumericVector A = closure_C(empty, I);




}
