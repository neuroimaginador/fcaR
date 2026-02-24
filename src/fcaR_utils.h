#ifndef FCAR_UTILS_H
#define FCAR_UTILS_H

#include <Rcpp.h>
#include "fcaR_bitset.h"

namespace fcaR {

inline std::vector<FastBitset> matrix_to_bitset_cols(const Rcpp::IntegerMatrix& I) {
    int nO = I.nrow();
    int nA = I.ncol();
    std::vector<FastBitset> cols(nA);
    for (int j = 0; j < nA; ++j) {
        cols[j].init(nO);
        for (int i = 0; i < nO; ++i) {
            if (I(i, j) != 0) cols[j].set(i);
        }
    }
    return cols;
}

inline std::vector<FastBitset> matrix_to_bitset_rows(const Rcpp::IntegerMatrix& I) {
    int nO = I.nrow();
    int nA = I.ncol();
    std::vector<FastBitset> rows(nO);
    for (int i = 0; i < nO; ++i) {
        rows[i].init(nA);
        for (int j = 0; j < nA; ++j) {
            if (I(i, j) != 0) rows[i].set(j);
        }
    }
    return rows;
}

} // namespace fcaR

#endif // FCAR_UTILS_H
