#include <Rcpp.h>
#include "implication_tree.h"
using namespace Rcpp;


double cardinal(SparseVector A);

SparseVector setdifference(SparseVector x,
                           SparseVector y);
void setdifference(SparseVector x,
                   SparseVector y,
                   SparseVector* res);

SparseVector compute_intent (SparseVector V,
                             NumericMatrix I);
SparseVector compute_intent (SparseVector V,
                             double* I,
                             int n_objects,
                             int n_attributes);
void compute_intent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes);

S4 compute_intent(S4 V, NumericMatrix I);
S4 compute_intent2(S4 V, NumericMatrix I);

SparseVector compute_extent (SparseVector V,
                             NumericMatrix I);
SparseVector compute_extent (SparseVector V,
                             double* I,
                             int n_objects,
                             int n_attributes);
void compute_extent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes);

S4 compute_extent(S4 V, NumericMatrix I);

SparseVector compute_closure (SparseVector V,
                              NumericMatrix I);
SparseVector compute_closure (SparseVector V,
                              double* I,
                              int n_objects,
                              int n_attributes);
void compute_closure (SparseVector* B,
                      SparseVector V,
                      double* I,
                      int n_objects,
                      int n_attributes);

S4 compute_closure(S4 V, NumericMatrix I);

void is_subset(SparseVector A,
               const struct ImplicationTree t,
               IntArray *res,
               bool* black_list);

void setunion(SparseVector RHS,
              IntArray subsets,
              SparseVector *res2);

void setunion2(SparseVector x,
               SparseVector y,
               SparseVector *res);
