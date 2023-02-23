#include <Rcpp.h>
#include "Logics.h"
using namespace Rcpp;

typedef double (*LogicOperator)(double, double);

// Zadeh
// [[Rcpp::export]]
double tnorm_Zadeh(double x, double y) {

  return (x <= y) ? x : y;

}

// [[Rcpp::export]]
double implication_Zadeh(double x, double y) {

  return (x <= y) ? 1 : y;

}

// Lukasiewicz
// [[Rcpp::export]]
double tnorm_Lukasiewicz(double x, double y) {

  return (0 <= x + y - 1) ? x + y - 1 : 0;

}

// [[Rcpp::export]]
double implication_Lukasiewicz(double x, double y) {

  return (1 - x + y <= 1) ? 1 - x + y : 1;

}


// Godel
// [[Rcpp::export]]
double tnorm_Godel(double x, double y) {

  return (x <= y) ? x : y;

}

// [[Rcpp::export]]
double implication_Godel(double x, double y) {

  return (x <= y) ? 1 : y;

}

// Product
// [[Rcpp::export]]
double tnorm_Product(double x, double y) {

  return x * y;

}

// [[Rcpp::export]]
double implication_Product(double x, double y) {

  return (x <= y) ? 1: y / x;

}

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
