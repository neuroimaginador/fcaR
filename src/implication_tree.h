#include <Rcpp.h>
#include "vector_operations.h"
using namespace Rcpp;

// Implication Tree

struct ImplicationTree {

  IntArray COUNT;
  DoubleArray CARD;
  DoubleArray DEGREE[5000];
  IntArray LIST[5000];
  int n_implications;
  int n_attributes;

};

// static void
//   _finalizer(SEXP ext);


void initImplicationTree(struct ImplicationTree *t, int n_attributes);

// void printImplicationTree(SEXP ext);

// SEXP createImplicationTree(int n_attributes);

void addImplicationToTree(struct ImplicationTree *t, SparseVector A);

// void addImplicationToTree_XPtr(SEXP ext, S4 A);
