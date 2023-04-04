#include <Rcpp.h>
#include "vector_operations.h"
using namespace Rcpp;

//////////////////////////////
// Sparse Vector Operations //
//////////////////////////////


void initArray(IntArray *a, size_t initialSize) {
  a->array = (int *)calloc(initialSize, sizeof(int));
  a->used = 0;
  a->size = initialSize;
}

// void printArray(IntArray a) {
//
//   Rprintf("Array:\n");
//   Rprintf("Used / Size = %u / %u\n", a.used, a.size);
//   for (int i = 0; i < a.used; i++) {
//
//     Rprintf("%d ", a.array[i]);
//
//   }
//
//   Rprintf("\n");
//
// }

void insertArray(IntArray *a, int element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size
  if (a->used == a->size) {
    a->size *= 2;
    int* tmp = (int *)realloc(a->array, a->size * sizeof(int));

    if (tmp != NULL) {

      a->array = tmp;
    }

    for (size_t i = a->used; i < a->size; i++) {

      a->array[i] = 0;

    }

  }
  a->array[a->used++] = element;
}

void freeArray(IntArray *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}

void printArray(IntArray a) {

  Rcout << "(";
  if (a.used > 0) {

    for (int i = 0; i < a.used; i++) {

      Rcout << a.array[i] << ", ";

    }

  }

  Rcout << ")" << std::endl;

}


void initArray(DoubleArray *a, size_t initialSize) {
  a->array = (double *)calloc(initialSize, sizeof(double));

  a->used = 0;
  a->size = initialSize;
}

void reinitArray(DoubleArray *a) {

  a->used = 0;

}
void reinitArray(IntArray *a) {

  a->used = 0;

}

// void printArray(DoubleArray a) {
//
//   Rprintf("Array:\n");
//   Rprintf("Used / Size = %u / %u\n", a.used, a.size);
//   for (int i = 0; i < a.used; i++) {
//
//     Rprintf("%f ", a.array[i]);
//
//   }
//
//   Rprintf("\n");
//
// }

void insertArray(DoubleArray *a, double element) {
  // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
  // Therefore a->used can go up to a->size
  if (a->used == a->size) {
    a->size *= 2;
    double* tmp = (double *)realloc(a->array, a->size * sizeof(double));

    if (tmp != NULL) {

      a->array = tmp;

    }

    for (size_t i = a->used; i < a->size; i++) {

      a->array[i] = 0;

    }

  }
  a->array[a->used++] = element;
}

void freeArray(DoubleArray *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}

void printArray(DoubleArray a) {

  Rcout << "(";
  if (a.used > 0) {

    for (int i = 0; i < a.used; i++) {

      Rcout << a.array[i] << ", ";

    }

  }

  Rcout << ")" << std::endl;

}

void initVector(SparseVector *a, size_t initialSize) {

  initArray(&(a->p), initialSize);
  initArray(&(a->i), initialSize);
  initArray(&(a->x), initialSize);
  a->length = initialSize;

}

void initMatrix(SparseVector *a, size_t nrow) {

  initArray(&(a->p), nrow * 100000);
  initArray(&(a->i), nrow * 100000);
  initArray(&(a->x), nrow * 100000);
  a->length = nrow;

}

void reinitVector(SparseVector *a) {

  reinitArray(&(a->i));
  reinitArray(&(a->x));

}

void freeVector(SparseVector *a) {

  freeArray(&(a->i));
  freeArray(&(a->p));
  freeArray(&(a->x));

  a->length = 0;

}

void printVector(SparseVector A, Rcpp::StringVector attrs) {

  Rprintf("{");

  for (size_t i = 0; i < A.i.used - 1; i++) {

    if (A.x.array[i] < 1) {

      Rcout << attrs[A.i.array[i]] << " [" << A.x.array[i] << "], ";

    } else {

      Rcout << attrs[A.i.array[i]] << ", ";

    }

  }

  int end = A.i.used - 1;

  if (end >= 0) {

    if (A.x.array[end] < 1) {

      Rcout << attrs[A.i.array[end]] << " [" << A.x.array[end] << "]";

    } else {

      Rcout << attrs[A.i.array[end]];

    }

  }

  Rprintf("}");

}


// void printVector(SparseVector A) {
//
//   Rprintf("{");
//
//   for (size_t i = 0; i < A.i.used - 1; i++) {
//
//     if (A.x.array[i] < 1) {
//
//       Rcout << attrs[A.i.array[i]] << " [" << A.x.array[i] << "], ";
//
//     } else {
//
//       Rcout << attrs[A.i.array[i]] << ", ";
//
//     }
//
//   }
//
//   int end = A.i.used - 1;
//
//   if (end >= 0) {
//
//     if (A.x.array[end] < 1) {
//
//       Rcout << attrs[A.i.array[end]] << " [" << A.x.array[end] << "]";
//
//     } else {
//
//       Rcout << attrs[A.i.array[end]];
//
//     }
//
//   }
//
//   Rprintf("}");
//
// }

void printImpl(SparseVector A,
               SparseVector B,
               Rcpp::StringVector attrs) {

  printVector(A, attrs);
  Rprintf(" -> ");
  printVector(B, attrs);
  Rprintf("\n");


}

void assignUsed(IntArray *a, const size_t n) {

  a->used = n;

}

void assignUsed(DoubleArray *a, const size_t n) {

  a->used = n;

}

void cloneVector(SparseVector *a, SparseVector b) {

  reinitVector(a);
  // freeVector(a);
  // initVector(a, b.x.size);

  if (b.i.used > 0) {

    // std::copy(&b.i.array[0], &b.i.array[b.i.used], a->i.array);
    // std::copy(&b.x.array[0], &b.x.array[b.i.used], a->x.array);

    memcpy(a->i.array, b.i.array, b.i.used * sizeof(int));
    memcpy(a->x.array, b.x.array, b.i.used * sizeof(double));

  }

  assignUsed(&(a->i), b.i.used);
  assignUsed(&(a->x), b.x.used);

}

void add_column(SparseVector *a, SparseVector b) {

  if (a->p.used > 0) {

    int last_p = a->p.array[a->p.used - 1];

    int added = 0;

    for (size_t i = 0; i < b.i.used; i++) {

      if (b.x.array[i] > 0) {

        insertArray(&(a->i), b.i.array[i]);
        insertArray(&(a->x), b.x.array[i]);
        added++;

      }

    }

    insertArray(&(a->p), last_p + added);

  } else {

    int added = 0;
    for (size_t i = 0; i < b.i.used; i++) {

      if (b.x.array[i] > 0) {

        insertArray(&(a->i), b.i.array[i]);
        insertArray(&(a->x), b.x.array[i]);
        added++;

      }

    }

    insertArray(&(a->p), 0);
    insertArray(&(a->p), added);

  }


}

SparseVector S4toSparse(S4 A) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  SparseVector V;
  initVector(&V, adims[0]);

  for (size_t i = 0; i < ai.size(); i++) {

    insertArray(&(V.i), ai[i]);
    insertArray(&(V.x), ax[i]);

  }
  insertArray(&(V.p), 0);

  if (V.i.used > 0) {

    insertArray(&(V.p), V.i.used);

  } else {

    insertArray(&(V.p), 0);

  }

  return V;

}

S4 SparseToS4(SparseVector V) {

  S4 res("dgCMatrix");

  std::vector<int> i;
  std::vector<double> x;
  IntegerVector dims(2);
  std::vector<int> p;

  for (size_t j = 0; j < V.i.used; j++) {

    i.push_back(V.i.array[j]);
    x.push_back(V.x.array[j]);

  }

  p.push_back(0);

  if (V.p.used > 0) {

    for (size_t j = 0; j < V.p.used; j++) {

      p.push_back(V.p.array[j]);

    }

  }

  dims[0] = V.length;
  dims[1] = V.p.used;

  res.slot("x") = x;
  res.slot("i") = i;
  res.slot("Dim") = dims;
  res.slot("p") = p;

  return(res);

}

S4 SparseToS4_fast(SparseVector V) {

  S4 res("dgCMatrix");

  IntegerVector i(V.i.used);
  NumericVector x(V.x.used);
  IntegerVector dims(2);
  IntegerVector p(V.p.used);

  if (V.i.used > 0) {

    memcpy(i.begin(), V.i.array, V.i.used * sizeof(int));
    memcpy(x.begin(), V.x.array, V.x.used * sizeof(double));

  }

  if (V.p.used > 0) {

    memcpy(&(p[0]), V.p.array, V.p.used * sizeof(int));

  }

  dims[0] = V.length;
  dims[1] = V.p.used - 1;

  res.slot("x") = x;
  res.slot("i") = i;
  res.slot("Dim") = dims;
  res.slot("p") = p;

  return(res);

}

SparseVector set_difference_sparse(IntegerVector xi,
                                   IntegerVector xp,
                                   NumericVector xx,
                                   IntegerVector yi,
                                   IntegerVector yp,
                                   NumericVector yx,
                                   int number) {

  SparseVector res;
  initVector(&res, number);

  int my_p = 0;

  // Rcout << "x.p.used = " << xp.size() << std::endl;

  insertArray(&(res.p), 0);

  for (size_t p = 0; p < xp.size() - 1; p++) {

    // Rcout << "Added column with " << my_p << std::endl;

    int init_x = xp[p], end_x = xp[p + 1];
    int init_y = yp[p], end_y = yp[p + 1];

    for (size_t i = init_x; i < end_x; i++) {

      bool add = true;

      // size_t j = init_y;
      //
      // while ((j < end_y) && (yi[j] < xi[i])) {
      //
      //   j++;
      //
      // }
      //
      // if (yi[j] == xi[i]) {
      //
      //   if (yx[j] >= xx[i]) {
      //
      //     add = false;
      //
      //   }
      // }

      for (size_t j = init_y; j < end_y; j++) {

        if (yi[j] > xi[i]) break;

        if (xi[i] == yi[j]) {

          if (yx[j] >= xx[i]) {

            add = false;
            break;

          }

        }

      }

      if (add) {

        my_p++;

        // Rcout << "Added element " << my_p << std::endl;

        insertArray(&(res.i), xi[i]);
        insertArray(&(res.x), xx[i]);

        // Rcout << xi[i] << std::endl;

      }

    }

    insertArray(&(res.p), my_p);

  }

  return res;

}

// [[Rcpp::export]]
S4 set_difference(IntegerVector xi,
                  IntegerVector xp,
                  NumericVector xx,
                  IntegerVector yi,
                  IntegerVector yp,
                  NumericVector yx,
                  int number) {

  SparseVector res = set_difference_sparse(xi, xp, xx,
                                           yi, yp, yx,
                                           number);

  S4 res2 = SparseToS4_fast(res);

  freeVector(&res);

  return res2;

}

SparseVector set_difference_sparse1(IntegerVector xi,
                                    IntegerVector xp,
                                    NumericVector xx,
                                    IntegerVector yi,
                                    IntegerVector yp,
                                    NumericVector yx,
                                    int number) {

  SparseVector res;
  initVector(&res, number);

  int my_p = 0;

  // Rcout << "x.p.used = " << xp.size() << std::endl;

  insertArray(&(res.p), 0);

  for (size_t p = 0; p < xp.size() - 1; p++) {

    // Rcout << "Added column with " << my_p << std::endl;

    int init_x = xp[p], end_x = xp[p + 1];
    int init_y = yp[0], end_y = yp[1];

    for (size_t i = init_x; i < end_x; i++) {

      bool add = true;

      // size_t j = init_y;
      //
      // while ((j < end_y) && (yi[j] < xi[i])) {
      //
      //   j++;
      //
      // }
      //
      // if (yi[j] == xi[i]) {
      //
      //   if (yx[j] >= xx[i]) {
      //
      //     add = false;
      //
      //   }
      // }

      for (size_t j = init_y; j < end_y; j++) {

        if (yi[j] > xi[i]) break;

        if (xi[i] == yi[j]) {

          if (yx[j] >= xx[i]) {

            add = false;
            break;

          }

        }

      }

      if (add) {

        my_p++;

        // Rcout << "Added element " << my_p << std::endl;

        insertArray(&(res.i), xi[i]);
        insertArray(&(res.x), xx[i]);

      }

    }

    insertArray(&(res.p), my_p);

  }

  return res;

}

// [[Rcpp::export]]
S4 set_difference_single(IntegerVector xi,
                         IntegerVector xp,
                         NumericVector xx,
                         IntegerVector yi,
                         IntegerVector yp,
                         NumericVector yx,
                         int number) {

  SparseVector res = set_difference_sparse1(xi, xp, xx,
                                            yi, yp, yx,
                                            number);

  S4 res2 = SparseToS4_fast(res);

  freeVector(&res);

  return res2;

}


SparseVector set_intersection_sparse1(IntegerVector xi,
                                    IntegerVector xp,
                                    NumericVector xx,
                                    IntegerVector yi,
                                    IntegerVector yp,
                                    NumericVector yx,
                                    int number) {

  SparseVector res;
  initVector(&res, number);

  int my_p = 0;

  // Rcout << "x.p.used = " << xp.size() << std::endl;

  insertArray(&(res.p), 0);

  for (size_t p = 0; p < xp.size() - 1; p++) {

    // Rcout << "Added column with " << my_p << std::endl;

    int init_x = xp[p], end_x = xp[p + 1];
    int init_y = yp[0], end_y = yp[1];

    for (size_t i = init_x; i < end_x; i++) {

      bool add = false;

      for (size_t j = init_y; j < end_y; j++) {

        if (yi[j] > xi[i]) break;

        if (xi[i] == yi[j]) {

          double val = (xx[i] > yx[j]) ? yx[j] : xx[i];

          if (val > 0) {

            my_p++;

            // Rcout << "Added element " << my_p << std::endl;

            insertArray(&(res.i), xi[i]);
            insertArray(&(res.x), val);

          }

        }

      }

    }

    insertArray(&(res.p), my_p);

  }

  return res;

}

// [[Rcpp::export]]
S4 set_intersection_single(IntegerVector xi,
                         IntegerVector xp,
                         NumericVector xx,
                         IntegerVector yi,
                         IntegerVector yp,
                         NumericVector yx,
                         int number) {

  SparseVector res = set_intersection_sparse1(xi, xp, xx,
                                            yi, yp, yx,
                                            number);

  S4 res2 = SparseToS4_fast(res);

  freeVector(&res);

  return res2;

}

NumericVector as_vector(SparseVector v) {

  NumericVector x(v.length);

  for (int i = 0; i < v.i.used; i++) {

    x[v.i.array[i]] = v.x.array[i];

  }

  return(x);

}

SparseVector as_sparse(NumericVector v) {

  SparseVector res;
  initVector(&res, v.size());

  for (int i = 0; i < v.size(); i++) {

    if (v[i] > 0) {

      insertArray(&(res.i), i);
      insertArray(&(res.x), v[i]);

    }

  }

  return res;

}

SparseVector as_sparse(double* v, int length) {

  SparseVector res;
  initVector(&res, length);

  for (int i = 0; i < length; i++) {

    if (v[i] > 0) {

      insertArray(&(res.i), i);
      insertArray(&(res.x), v[i]);

    }

  }

  return res;

}

SparseVector as_sparse(double* v,
                       int nrow, int ncol,
                       int j) {

  SparseVector res;
  initVector(&res, nrow);

  for (int i = 0; i < nrow; i++) {

    if (v[j * nrow + i] > 0) {

      insertArray(&(res.i), i);
      insertArray(&(res.x), v[j * nrow + i]);

    }

  }

  return res;

}

void as_sparse(SparseVector *res,
               double* v,
               int nrow, int ncol,
               int j) {

  // SparseVector res;
  // initVector(&res, nrow);

  for (int i = 0; i < nrow; i++) {

    if (v[j * nrow + i] > 0) {

      insertArray(&(res->i), i);
      insertArray(&(res->x), v[j * nrow + i]);

    }

  }


}

double get_element(SparseVector v, int n) {

  if (n > v.length - 1) {

    return 0;

  }

  double res = 0;

  for (int i = 0; i < v.i.used; i++) {

    if (v.i.array[i] > n)
      break;

    if (v.i.array[i] == n) {

      res = v.x.array[i];
      break;

    }

  }

  return res;

}
