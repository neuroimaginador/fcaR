#ifndef __BINARY_OPERATIONS_H
#define __BINARY_OPERATIONS_H

#include <Rcpp.h>
#include "vector_operations.h"

typedef struct {
  IntArray i;
  size_t length;
} BinarySparseVector;

// // Functions for IntArray
// void initArray(IntArray *a, size_t initialSize);
// void reinitArray(IntArray *a);
// void insertArray(IntArray *a, int element);
// void freeArray(IntArray *a);
// void printArray(IntArray a);

// Functions for BinarySparseVector
void initVector(BinarySparseVector *a, size_t initialSize);
void freeVector(BinarySparseVector *a);
void cloneVector(BinarySparseVector *dst, const BinarySparseVector *src);


// Binary Galois operators
void binary_intent(BinarySparseVector *res,
                   const IntArray* extent,
                   const Rcpp::IntegerMatrix& I);

void binary_compute_closure(BinarySparseVector *res,
                            const BinarySparseVector* A,
                            const Rcpp::IntegerMatrix& I);


#endif // __BINARY_OPERATIONS_H
