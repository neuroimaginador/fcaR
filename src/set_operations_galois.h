#include <Rcpp.h>
#include "implication_tree.h"
#include "Logics.h"
using namespace Rcpp;

// ------------------ Tipos auxiliares ------------------

typedef void (*GaloisOperator)(SparseVector*, SparseVector,
              double*, int, int,
              LogicOperator, LogicOperator);

// ------------------ Funciones básicas ------------------

double cardinal(SparseVector A);

SparseVector setdifference(SparseVector x,
                           SparseVector y);
void setdifference(SparseVector x,
                   SparseVector y,
                   SparseVector* res);

void setunion(SparseVector RHS,
              IntArray subsets,
              SparseVector *res2);

void setunion2(SparseVector x,
               SparseVector y,
               SparseVector *res);

void is_subset(SparseVector A,
               const struct ImplicationTree t,
               IntArray *res,
               bool* black_list);

// ------------------ Intent ------------------

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
                     int n_attributes,
                     LogicOperator tnorm,
                     LogicOperator implication);
void compute_intent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes);

S4 compute_intent(S4 V, NumericMatrix I);
S4 compute_intent2(S4 V, NumericMatrix I);
S4 compute_intent(S4 V, NumericMatrix I,
                  String connection = "standard",
                  String name = "Zadeh");

// ------------------ Extent ------------------

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
                     int n_attributes,
                     LogicOperator tnorm,
                     LogicOperator implication);
void compute_extent (SparseVector *R,
                     SparseVector V,
                     double* I,
                     int n_objects,
                     int n_attributes);

S4 compute_extent(S4 V, NumericMatrix I);
S4 compute_extent(S4 V, NumericMatrix I,
                  String connection = "standard",
                  String name = "Zadeh");

// ------------------ Closure ------------------

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
                      int n_attributes,
                      GaloisOperator extent_f,
                      GaloisOperator intent_f,
                      LogicOperator tnorm,
                      LogicOperator implication);
void compute_closure (SparseVector* B,
                      SparseVector V,
                      double* I,
                      int n_objects,
                      int n_attributes);

S4 compute_closure(S4 V, NumericMatrix I);
S4 compute_closure(S4 V, NumericMatrix I,
                   String connection = "standard",
                   String name = "Zadeh");

// ------------------ Flechas (añadidas desde la oficial) ------------------

void compute_upright_arrow (SparseVector *R,
                            SparseVector V,
                            double* I,
                            int n_objects,
                            int n_attributes,
                            LogicOperator tnorm,
                            LogicOperator implication);
S4 compute_upright_arrow(S4 V, NumericMatrix I, String name);

void compute_downleft_arrow (SparseVector *R,
                             SparseVector V,
                             double* I,
                             int n_objects,
                             int n_attributes,
                             LogicOperator tnorm,
                             LogicOperator implication);
S4 compute_downleft_arrow(S4 V, NumericMatrix I, String name);

void compute_upleft_arrow (SparseVector *R,
                           SparseVector V,
                           double* I,
                           int n_objects,
                           int n_attributes,
                           LogicOperator tnorm,
                           LogicOperator implication);
S4 compute_upleft_arrow(S4 V, NumericMatrix I, String name);

void compute_downright_arrow (SparseVector *R,
                              SparseVector V,
                              double* I,
                              int n_objects,
                              int n_attributes,
                              LogicOperator tnorm,
                              LogicOperator implication);
S4 compute_downright_arrow(S4 V, NumericMatrix I, String name);

// ------------------ Auxiliares ------------------

GaloisOperator get_intent_function(String connection);
GaloisOperator get_extent_function(String connection);
