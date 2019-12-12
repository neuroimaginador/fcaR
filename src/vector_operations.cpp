#include <Rcpp.h>
#include "vector_operations.h"
using namespace Rcpp;

//////////////////////////////
// Sparse Vector Operations //
//////////////////////////////


void initArray(IntArray *a, size_t initialSize) {
  a->array = (int *)malloc(initialSize * sizeof(int));
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
    a->array = (int *)realloc(a->array, a->size * sizeof(int));
  }
  a->array[a->used++] = element;
}

void freeArray(IntArray *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}


void initArray(DoubleArray *a, size_t initialSize) {
  a->array = (double *)malloc(initialSize * sizeof(double));
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
    a->array = (double *)realloc(a->array, a->size * sizeof(double));
  }
  a->array[a->used++] = element;
}

void freeArray(DoubleArray *a) {
  free(a->array);
  a->array = NULL;
  a->used = a->size = 0;
}


void initVector(SparseVector *a, size_t initialSize) {

  initArray(&(a->p), initialSize);
  initArray(&(a->i), initialSize);
  initArray(&(a->x), initialSize);
  a->length = initialSize;

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

  for (int i = 0; i < A.i.used - 1; i++) {

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

  freeVector(a);
  initVector(a, b.length);

  if (b.i.used > 0) {

    std::copy(&b.i.array[0], &b.i.array[b.i.used], a->i.array);
    std::copy(&b.x.array[0], &b.x.array[b.i.used], a->x.array);

    //    memcpy(a->i.array, b.i.array, b.i.used * sizeof(int));
    //    memcpy(a->x.array, b.x.array, b.i.used * sizeof(double));

  }

  assignUsed(&(a->i), b.i.used);
  assignUsed(&(a->x), b.x.used);


}

void add_column(SparseVector *a, SparseVector b) {

  if (a->p.used > 0) {

    int last_p = a->p.array[a->p.used - 1];

    for (int i = 0; i < b.i.used; i++) {

      insertArray(&(a->i), b.i.array[i]);
      insertArray(&(a->x), b.x.array[i]);

    }

    insertArray(&(a->p), last_p + b.i.used);

  } else {

    for (int i = 0; i < b.i.used; i++) {

      insertArray(&(a->i), b.i.array[i]);
      insertArray(&(a->x), b.x.array[i]);

    }

    insertArray(&(a->p), 0);
    insertArray(&(a->p), b.i.used);

  }


}

SparseVector S4toSparse(S4 A) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  SparseVector V;
  initVector(&V, adims[0]);

  for (int i = 0; i < ai.size(); i++) {

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

  for (int j = 0; j < V.i.used; j++) {

    i.push_back(V.i.array[j]);
    x.push_back(V.x.array[j]);

  }

  p.push_back(0);

  if (V.p.used > 0) {

    for (int j = 0; j < V.p.used; j++) {

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
