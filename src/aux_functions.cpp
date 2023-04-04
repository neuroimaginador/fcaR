#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;

void get_column(SparseVector* A,
                SparseVector qA,
                int id_col) {

  int cont = 0;
  for (int i = qA.p.array[id_col]; i < qA.p.array[id_col + 1]; i++) {

    insertArray(&(A->i), qA.i.array[i]);
    insertArray(&(A->x), qA.x.array[i]);
    cont++;

  }

  insertArray(&(A->p), 0);
  insertArray(&(A->p), cont);

}

// [[Rcpp::export]]
void print_matrix(NumericMatrix I) {

  for (int i = 0; i < I.nrow(); i++) {

    for (int j = 0; j < I.ncol(); j++) {

      Rcout << I(i, j) << " ";

    }

    Rcout << std::endl;

  }

}

// [[Rcpp::export]]
void print_vector(NumericVector I, int sz) {

  if (sz > I.size())
    sz = I.size();

  for (int i = 0; i < sz; i++) {

    Rcout << I[i] << " ";

  }

  Rcout << std::endl;

}

// [[Rcpp::export]]
double get_element_array(NumericVector I,
                         int i, int j, int k) {

  IntegerVector res(I.attr("dim"));

  int linear_index = k * res[0] * res[1] + j * res[0] + i;

  return I[linear_index];

}

NumericVector zadeh_I(double x, NumericVector y) {

  int n = y.size();
  NumericVector res(y);

  for (int i = 0; i < n; i++) {

    if (x <= y[i]) {

      res[i] = 1;

    }

  }

  return res;

}

void zadeh_I(double x, SparseVector *A) {

  for (size_t i = 0; i < A->i.used; i++) {

    if (A->x.array[i] >= x) {

      A->x.array[i] = 1;

    }

  }

}

void intersect(SparseVector *A, SparseVector B) {

  size_t i, j = 0, to_write = 0;

  for (i = 0; i < A->i.used; i++) {

    int ix = A->i.array[i];

    while ((B.i.array[j] < ix) & (j < B.i.used)) j++;

    if (j >= B.i.used) break;

    if (B.i.array[j] == ix) {

      if (B.x.array[j] < A->x.array[i]) {

        A->x.array[to_write] = B.x.array[j];
        A->i.array[to_write] = ix;
        to_write++;

      } else {

        A->x.array[to_write] = A->x.array[i];
        A->i.array[to_write] = ix;
        to_write++;

      }

    }

  }

  A->i.used = to_write;
  A->x.used = to_write;

  if (A->p.used == 2)
    A->p.array[1] = to_write;

}
