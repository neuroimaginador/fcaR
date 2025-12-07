#include "implication_tree.h"
#include <Rcpp.h>
using namespace Rcpp;

// Implication Tree
//
// A Trie-like data structure to store a set of implications.
// It is used to efficiently query for implications or check for
// subsets/closures.
//
// Each node in the tree represents an attribute.
// The tree allows for:
// - Fast insertion of implications.
// - Efficient traversal to find implications applicable to a given set of
// attributes.
//
// n_attributes: Total number of attributes in the context.
// n_implications: Total number of implications stored.
// CARD, COUNT: Arrays likely used to store statistics or aggregated values at
// nodes. LIST: Adjacency list or similar structure to manage child nodes or
// implication indices. DEGREE: Likely stores degrees or weights associated with
// implications. static void
//   _finalizer(SEXP ext)
//   {
//     struct ImplicationTree *ptr = (struct ImplicationTree*)
//     R_ExternalPtrAddr(ext); Rprintf("Destroying pointer.\n");
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
//   struct ImplicationTree *tree = (struct ImplicationTree*)
//   R_ExternalPtrAddr(ext);
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

  insertArray(&(t->CARD), 0.0);
  insertArray(&(t->COUNT), 0);

  t->n_implications = t->n_implications + 1;

  for (size_t i = 0; i < A.i.used; i++) {

    insertArray(&(t->LIST[A.i.array[i]]), new_idx);

    insertArray(&(t->DEGREE[A.i.array[i]]), A.x.array[i]);

    (t->CARD).array[new_idx] = (t->CARD).array[new_idx] + A.x.array[i];
    (t->COUNT).array[new_idx] = (t->COUNT).array[new_idx] + 1;
  }

  // Rprintf("Added with CARD = %f, COUNT = %u\n", (t->CARD).array[new_idx],
  // (t->COUNT).array[new_idx]);
}

// void addImplicationToTree_XPtr(SEXP ext, S4 A) {
//
//   // printImplicationTree(ext);
//
//   SparseVector S = S4toSparse(A);
//   struct ImplicationTree *tree = (struct ImplicationTree*)
//   R_ExternalPtrAddr(ext); addImplicationToTree(tree, S);
//
// }

void freeImplicationTree(struct ImplicationTree *t) {

  freeArray(&(t->CARD));
  freeArray(&(t->COUNT));

  for (int i = 0; i < t->n_attributes; i++) {

    freeArray(&(t->LIST[i]));
    freeArray(&(t->DEGREE[i]));
  }

  // free(t->DEGREE);
  // free(t->LIST);
  // Free(t);
}
