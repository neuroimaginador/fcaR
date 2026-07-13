#include "Logics.h"
#include <Rcpp.h>
using namespace Rcpp;

typedef double (*LogicOperator)(double, double);

// Lukasiewicz Logic
// T-norm: $T(x, y) = \max(0, x + y - 1)$
// Implication: $I(x, y) = \min(1, 1 - x + y)$
// [[Rcpp::export]]
double tnorm_Lukasiewicz(double x, double y) {
  return (0 <= x + y - 1) ? x + y - 1 : 0;
}

// [[Rcpp::export]]
double implication_Lukasiewicz(double x, double y) {
  return (1 - x + y <= 1) ? 1 - x + y : 1;
}

// Godel Logic
// T-norm: $T(x, y) = \min(x, y)$
// Implication: $I(x, y) = 1$ if $x \le y$, else $y$.
// [[Rcpp::export]]
double tnorm_Godel(double x, double y) { return (x <= y) ? x : y; }

// [[Rcpp::export]]
double implication_Godel(double x, double y) { return (x <= y) ? 1 : y; }

// Product Logic (Goguen)
// T-norm: $T(x, y) = x \cdot y$
// Implication: $I(x, y) = 1$ if $x \le y$, else $y/x$.
// [[Rcpp::export]]
double tnorm_Product(double x, double y) { return x * y; }

// [[Rcpp::export]]
double implication_Product(double x, double y) { return (x <= y) ? 1 : y / x; }

LogicOperator get_implication(String name) {
  if (name == "Lukasiewicz") {
    return implication_Lukasiewicz;
  }

  if (name == "Godel") {
    return implication_Godel;
  }

  if (name == "Product") {
    return implication_Product;
  }

  return NULL;
}

LogicOperator get_tnorm(String name) {
  if (name == "Lukasiewicz") {
    return tnorm_Lukasiewicz;
  }

  if (name == "Godel") {
    return tnorm_Godel;
  }

  if (name == "Product") {
    return tnorm_Product;
  }

  // error handling
  return NULL;
}

// [[Rcpp::export]]
StringVector available_logics() {
  StringVector res(3);
  res[0] = "Lukasiewicz";
  res[1] = "Godel";
  res[2] = "Product";

  return res;
}
