#ifndef INCLOSE_BINARY_H
#define INCLOSE_BINARY_H

#include <Rcpp.h>
#include "aux_functions.h"

List InClose_binary(NumericMatrix I,
                    StringVector attrs,
                    bool verbose = false);

#endif // INCLOSE_BINARY_H
