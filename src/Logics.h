#include <Rcpp.h>
using namespace Rcpp;

typedef double (*LogicOperator)(double, double);

double tnorm_Lukasiewicz(double x, double y);
double implication_Lukasiewicz(double x, double y);
double tnorm_Godel(double x, double y);
double implication_Godel(double x, double y);
double tnorm_Product(double x, double y);
double implication_Product(double x, double y);
LogicOperator get_implication(String name);
LogicOperator get_tnorm(String name);
