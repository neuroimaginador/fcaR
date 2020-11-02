#include <Rcpp.h>
#include "implication_tree.h"
using namespace Rcpp;

// Implication Tree


// static void
//   _finalizer(SEXP ext)
//   {
//     struct ImplicationTree *ptr = (struct ImplicationTree*) R_ExternalPtrAddr(ext);
//     Rprintf("Destroying pointer.\n");
//     freeArray(&(ptr->CARD));
//     freeArray(&(ptr->COUNT));
//     free(ptr->DEGREE);
//     free(ptr->LIST);
//     Free(ptr);
//
//   }


void initImplicationTree(struct ImplicationTree *t, int n_attributes) {

  initArray(&(t->CARD), n_attributes);
  initArray(&(t->COUNT), n_attributes);
  t->n_implications = 0;
  t->n_attributes = n_attributes;

  // Rprintf("Number of attributes: %u\n", t->n_attributes);
  // Rprintf("Number of implications: %d\n", t->n_implications);

  for (int i = 0; i < n_attributes; i++) {

    initArray(&(t->LIST[i]), n_attributes);
    initArray(&(t->DEGREE[i]), n_attributes);

  }

}

// void printImplicationTree(SEXP ext) {
//
//   struct ImplicationTree *tree = (struct ImplicationTree*) R_ExternalPtrAddr(ext);
//
//   Rprintf("ImplicationTree\n");
//   Rprintf("Number of attributes: %u\n", tree->n_attributes);
//   Rprintf("Number of implications: %d\n", tree->n_implications);
//
//   printArray(tree->CARD);
//   printArray(tree->COUNT);
//
// }
//
// SEXP createImplicationTree(int n_attributes) {
//
//   struct ImplicationTree *t = Calloc(1, struct ImplicationTree);
//   initImplicationTree(t, n_attributes);
//
//   SEXP ext = PROTECT(R_MakeExternalPtr(t, R_NilValue, R_NilValue));
//   R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
//   UNPROTECT(1);
//
//   return ext;
//
// }

void addImplicationToTree(struct ImplicationTree *t, SparseVector A) {

  int new_idx = t->n_implications;

  insertArray(&(t->CARD), 0);
  insertArray(&(t->COUNT), 0);

  t->n_implications = t->n_implications + 1;

  for (size_t i = 0; i < A.i.used; i++) {

    insertArray(&(t->LIST[A.i.array[i]]), new_idx);

    insertArray(&(t->DEGREE[A.i.array[i]]), A.x.array[i]);

    (t->CARD).array[new_idx] = (t->CARD).array[new_idx] + A.x.array[i];
    (t->COUNT).array[new_idx] = (t->COUNT).array[new_idx] + 1;

  }

  // Rprintf("Added with CARD = %f, COUNT = %u\n", (t->CARD).array[new_idx], (t->COUNT).array[new_idx]);

}

// void addImplicationToTree_XPtr(SEXP ext, S4 A) {
//
//   // printImplicationTree(ext);
//
//   SparseVector S = S4toSparse(A);
//   struct ImplicationTree *tree = (struct ImplicationTree*) R_ExternalPtrAddr(ext);
//   addImplicationToTree(tree, S);
//
// }
