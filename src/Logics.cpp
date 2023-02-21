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

LogicOperator get_implication(String name) {

  if (name == "Zadeh") {

    return implication_Zadeh;

  } else if (name == "Lukasiewicz") {

    return implication_Lukasiewicz;

  } else {

    // error handling
    return NULL;

  }

}

LogicOperator get_tnorm(String name) {

  if (name == "Zadeh") {

    return tnorm_Zadeh;

  } else if (name == "Lukasiewicz") {

    return tnorm_Lukasiewicz;

  } else {

    // error handling
    return NULL;

  }

}
