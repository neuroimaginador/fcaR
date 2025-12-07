#include "Logics.h"
#include <Rcpp.h>
using namespace Rcpp;

typedef double (*LogicOperator)(double, double);

// Zadeh Logic
// T-norm: $T(x, y) = \min(x, y)$
// Implication: $I(x, y) = 1$ if $x \le y$, else $y$ (Residuated implication not
// standard Zadeh, but usually Godel. Here it seems to be defined as: if x<=y
// then 1 else y?) Wait, Standard Zadeh implication is $I(x, y) = \max(1-x,
// \min(x, y))$ (Kleene-Dienes) or other variants. The code `return (x <= y) ? 1
// : y;` is actually the **Godel** implication! But the function is named
// `implication_Zadeh`. Let's explain what the code does: $x \to y =
// \begin{cases} 1 & \text{if } x \le y \\ y & \text{otherwise} \end{cases}$
// This is effectively the Gödel implication.
//
// [[Rcpp::export]]
double tnorm_Zadeh(double x, double y) { return (x <= y) ? x : y; }

// [[Rcpp::export]]
double implication_Zadeh(double x, double y) { return (x <= y) ? 1 : y; }

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

  if (name == "Zadeh") {

    return implication_Zadeh;
  }
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

  if (name == "Zadeh") {

    return tnorm_Zadeh;
  }
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

  StringVector res(4);
  res[0] = "Lukasiewicz";
  res[1] = "Zadeh";
  res[2] = "Godel";
  res[3] = "Product";

  return res;
}
