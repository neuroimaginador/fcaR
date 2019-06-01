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

// Functions used in different fuzzy logic families (sets package)

double zadeh_S(double x, double y) {

  if (x > y) return x;

  return y;

}

double zadeh_T(double x, double y) {

  if (x > y) return y;

  return x;

}

double zadeh_I(double x, double y) {

  if (x <= y) return 1;

  return y;

}

double drastic_S(double x, double y) {

  if (zadeh_T(x, y) == 0) return zadeh_S(x, y);

  return 1;

}

double drastic_T(double x, double y) {

  if (zadeh_S(x, y) == 1) return zadeh_T(x, y);

  return 0;

}

double product_S(double x, double y) {

  return x + y - x * y;

}

double product_T(double x, double y) {

  return x * y;

}

double product_I(double x, double y) {

  if (x > 0) return zadeh_T(y/x, 1);

  return 1;

}

double lukasiewicz_S(double x, double y) {

  return zadeh_T(x + y, 1);

}

double lukasiewicz_T(double x, double y) {

  return zadeh_S(x + y - 1, 0);

}

double lukasiewicz_I(double x, double y) {

  return zadeh_T(1 - x + y, 1);

}

double opposite(double x, double y) {

  return zadeh_S(0, x - y);

}

double set_diff(double x, double y) {

  if (x > y) return x;

  return 0;

}

// [[Rcpp::export]]
NumericVector apply_F_colwise(NumericMatrix x,
                              CharacterVector type,
                              double init_value) {

  int rows = x.nrow(), cols = x.ncol();

  NumericVector res(cols, init_value);

  double (*f)(double, double);

  if (type.size() > 0) {

    if (type[0] == "zadeh_T") f = zadeh_T;
    if (type[0] == "zadeh_S") f = zadeh_S;
    if (type[0] == "zadeh_I") f = zadeh_I;
    if (type[0] == "drastic_T") f = drastic_T;
    if (type[0] == "drastic_S") f = drastic_S;
    if (type[0] == "product_T") f = product_T;
    if (type[0] == "product_S") f = product_S;
    if (type[0] == "product_I") f = product_I;
    if (type[0] == "lukasiewicz_T") f = lukasiewicz_T;
    if (type[0] == "lukasiewicz_S") f = lukasiewicz_S;
    if (type[0] == "lukasiewicz_I") f = lukasiewicz_I;
    if (type[0] == "opposite") f = opposite;
    if (type[0] == "set_diff") f = set_diff;

    for (int i = 0; i < rows; i++) {

      for (int j = 0; j < cols; j++) {


        res[j] = f(res[j], x(i, j));

      }

    }

  }

  return res;

}

// [[Rcpp::export]]
NumericVector apply_F_rowwise(NumericMatrix x,
                              CharacterVector type,
                              double init_value) {

  int rows = x.nrow(), cols = x.ncol();

  NumericVector res(rows, init_value);

  double (*f)(double, double);

  if (type.size() > 0) {

    if (type[0] == "zadeh_T") f = zadeh_T;
    if (type[0] == "zadeh_S") f = zadeh_S;
    if (type[0] == "zadeh_I") f = zadeh_I;
    if (type[0] == "drastic_T") f = drastic_T;
    if (type[0] == "drastic_S") f = drastic_S;
    if (type[0] == "product_T") f = product_T;
    if (type[0] == "product_S") f = product_S;
    if (type[0] == "product_I") f = product_I;
    if (type[0] == "lukasiewicz_T") f = lukasiewicz_T;
    if (type[0] == "lukasiewicz_S") f = lukasiewicz_S;
    if (type[0] == "lukasiewicz_I") f = lukasiewicz_I;
    if (type[0] == "opposite") f = opposite;
    if (type[0] == "set_diff") f = set_diff;

    for (int j = 0; j < cols; j++) {

      for (int i = 0; i < rows; i++) {


        res[i] = f(res[i], x(i, j));

      }

    }

  }

  return res;

}

// [[Rcpp::export]]
NumericMatrix apply_F_rowwise_xy(NumericMatrix x,
                                 NumericVector y,
                                 CharacterVector type) {

  int rows = x.nrow(), cols = x.ncol();

  NumericMatrix res(rows, cols);

  double (*f)(double, double);

  if (type.size() > 0) {

    if (type[0] == "zadeh_T") f = zadeh_T;
    if (type[0] == "zadeh_S") f = zadeh_S;
    if (type[0] == "zadeh_I") f = zadeh_I;
    if (type[0] == "drastic_T") f = drastic_T;
    if (type[0] == "drastic_S") f = drastic_S;
    if (type[0] == "product_T") f = product_T;
    if (type[0] == "product_S") f = product_S;
    if (type[0] == "product_I") f = product_I;
    if (type[0] == "lukasiewicz_T") f = lukasiewicz_T;
    if (type[0] == "lukasiewicz_S") f = lukasiewicz_S;
    if (type[0] == "lukasiewicz_I") f = lukasiewicz_I;
    if (type[0] == "opposite") f = opposite;
    if (type[0] == "set_diff") f = set_diff;

    for (int j = 0; j < cols; j++) {

      for (int i = 0; i < rows; i++) {


        res(i, j) = f(x(i, j), y[i]);

      }

    }

  }

  return res;

}

// [[Rcpp::export]]
NumericMatrix apply_F_elementwise(NumericMatrix x,
                                 NumericMatrix y,
                                 CharacterVector type) {

  int rows = x.nrow(), cols = x.ncol();

  NumericMatrix res(rows, cols);

  double (*f)(double, double);

  if (type.size() > 0) {

    if (type[0] == "zadeh_T") f = zadeh_T;
    if (type[0] == "zadeh_S") f = zadeh_S;
    if (type[0] == "zadeh_I") f = zadeh_I;
    if (type[0] == "drastic_T") f = drastic_T;
    if (type[0] == "drastic_S") f = drastic_S;
    if (type[0] == "product_T") f = product_T;
    if (type[0] == "product_S") f = product_S;
    if (type[0] == "product_I") f = product_I;
    if (type[0] == "lukasiewicz_T") f = lukasiewicz_T;
    if (type[0] == "lukasiewicz_S") f = lukasiewicz_S;
    if (type[0] == "lukasiewicz_I") f = lukasiewicz_I;
    if (type[0] == "opposite") f = opposite;
    if (type[0] == "set_diff") f = set_diff;

    for (int j = 0; j < cols; j++) {

      for (int i = 0; i < rows; i++) {


        res(i, j) = f(x(i, j), y(i, j));

      }

    }

  }

  return res;

}
