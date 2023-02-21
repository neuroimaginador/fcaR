#include <Rcpp.h>
#include "set_operations_galois.h"

using namespace Rcpp;

void get_column(SparseVector* A,
                SparseVector qA,
                int id_col);

void print_matrix(NumericMatrix I);

void print_vector(NumericVector I, int sz);

double get_element_array(NumericVector I,
                         int i, int j, int k);

NumericVector zadeh_I(double x, NumericVector y);

void zadeh_I(double x, SparseVector *A);

void intersect(SparseVector *A, SparseVector B);

double tnorm_Zadeh(double x, double y);
double implication_Zadeh(double x, double y);
double tnorm_Lukasiewicz(double x, double y);
double implication_Lukasiewicz(double x, double y);
