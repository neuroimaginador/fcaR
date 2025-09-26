
#ifndef __VECTOR_OPERATIONS_H
#define __VECTOR_OPERATIONS_H

#include <Rcpp.h>
using namespace Rcpp;

// Integer vectors

typedef struct {
  int *array;
  size_t used;
  size_t size;
} IntArray;

void initArray(IntArray *a, size_t initialSize);
// void printArray(IntArray a);
void insertArray(IntArray *a, int element);
void freeArray(IntArray *a);
void printArray(IntArray a);

// Double vectors

typedef struct {
  double *array;
  size_t used;
  size_t size;
} DoubleArray;

typedef struct {
  IntArray p;
  IntArray i;
  DoubleArray x;
  size_t length;
} SparseVector;


void initArray(DoubleArray *a, size_t initialSize);
void reinitArray(DoubleArray *a);
void reinitArray(IntArray *a);
// void printArray(DoubleArray a);
void insertArray(DoubleArray *a, double element);
void freeArray(DoubleArray *a);
void printArray(DoubleArray a);


void initVector(SparseVector *a, size_t initialSize);
void initMatrix(SparseVector *a, size_t nrow);
void reinitVector(SparseVector *a);
void freeVector(SparseVector *a);
void printVector(SparseVector A, Rcpp::StringVector attrs);
void printImpl(SparseVector A,
               SparseVector B,
               Rcpp::StringVector attrs);

void assignUsed(IntArray *a, const size_t n);
void assignUsed(DoubleArray *a, const size_t n);

void cloneVector(SparseVector *a, SparseVector b);

void add_column(SparseVector *a, SparseVector b);

SparseVector S4toSparse(S4 A);
S4 SparseToS4(SparseVector V);
S4 SparseToS4_fast(SparseVector V);

NumericVector as_vector(SparseVector v);
SparseVector as_sparse(NumericVector v);
SparseVector as_sparse(double* v, int length);
SparseVector as_sparse(double* v,
                       int nrow, int ncol,
                       int j);
void as_sparse(SparseVector *res,
               double* v,
               int nrow, int ncol,
               int j);

double get_element(SparseVector v, int n);
#endif
