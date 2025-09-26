#include "binary_operations.h"

// // Functions for IntArray
// void initArray(IntArray *a, size_t initialSize) {
//   a->array = (int *)R_alloc(initialSize, sizeof(int));
//   a->used = 0;
//   a->size = initialSize;
// }
//
// void reinitArray(IntArray *a) {
//   a->used = 0;
// }
//
// void insertArray(IntArray *a, int element) {
//   if (a->used == a->size) {
//     size_t newSize = a->size * 2;
//     int *newArray = (int *)R_alloc(newSize, sizeof(int));
//     memcpy(newArray, a->array, a->used * sizeof(int));
//     a->array = newArray;
//     a->size = newSize;
//   }
//   a->array[a->used++] = element;
// }
//
//
// void freeArray(IntArray *a) {
//   // Memory allocated with R_alloc is garbage collected by R
//   // No need to free it manually
//   a->array = NULL;
//   a->used = a->size = 0;
// }
//
// void printArray(IntArray a) {
//   for (size_t i = 0; i < a.used; i++) {
//     Rprintf("%d ", a.array[i]);
//   }
//   Rprintf("\n");
// }

// Functions for BinarySparseVector
void initVector(BinarySparseVector *a, size_t initialSize) {
  initArray(&(a->i), initialSize);
  a->length = 0;
}

void freeVector(BinarySparseVector *a) {
  freeArray(&(a->i));
  a->length = 0;
}

void cloneVector(BinarySparseVector *dst, const BinarySparseVector *src) {
  initVector(dst, src->i.size);
  dst->length = src->length;
  for (size_t i = 0; i < src->i.used; ++i) {
    insertArray(&(dst->i), src->i.array[i]);
  }
}

// Binary Galois operators
void binary_extent(IntArray *res,
                   const BinarySparseVector* intent,
                   const Rcpp::IntegerMatrix& I) {
  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  reinitArray(res);

  for (int o = 0; o < n_objects; ++o) {
    bool object_in_extent = true;
    for (size_t i = 0; i < intent->i.used; ++i) {
      if (I(o, intent->i.array[i]) == 0) {
        object_in_extent = false;
        break;
      }
    }
    if (object_in_extent) {
      insertArray(res, o);
    }
  }
}

void binary_intent(BinarySparseVector *res,
                   const IntArray* extent,
                   const Rcpp::IntegerMatrix& I) {
  int n_attributes = I.ncol();
  reinitArray(&(res->i));

  for (int attr = 0; attr < n_attributes; ++attr) {
    bool attribute_in_intent = true;
    for (size_t i = 0; i < extent->used; ++i) {
      if (I(extent->array[i], attr) == 0) {
        attribute_in_intent = false;
        break;
      }
    }
    if (attribute_in_intent) {
      insertArray(&(res->i), attr);
    }
  }
}

void binary_compute_closure(BinarySparseVector *res,
                            const BinarySparseVector* A,
                            const Rcpp::IntegerMatrix& I) {
  IntArray extent;
  initArray(&extent, I.nrow());

  binary_extent(&extent, A, I);
  binary_intent(res, &extent, I);

  freeArray(&extent);
}
