#include <Rcpp.h>
#include "fcaR_bitset.h"

using namespace Rcpp;
using Bitset = fcaR::FastBitset;

// [[Rcpp::export]]
List binary_closure_cpp(S4 S_mat, S4 LHS_mat, S4 RHS_mat) {
    IntegerVector S_i = S_mat.slot("i");
    IntegerVector dim_LHS = LHS_mat.slot("Dim");
    int n_attrs = dim_LHS[0];
    int n_rules = dim_LHS[1];

    Bitset S; S.init(n_attrs);
    for (int i : S_i) {
        if (i > 0 && i <= n_attrs) S.set(i - 1);
    }

    IntegerVector LHS_i = LHS_mat.slot("i");
    IntegerVector LHS_p = LHS_mat.slot("p");
    IntegerVector RHS_i = RHS_mat.slot("i");
    IntegerVector RHS_p = RHS_mat.slot("p");

    std::vector<int> count(n_rules);
    std::vector<std::vector<int> > list_m(n_attrs);

    std::vector<int> update;
    for (int j = 0; j < n_rules; ++j) {
        int start = LHS_p[j];
        int end = LHS_p[j+1];
        count[j] = end - start;
        if (count[j] == 0) {
            // fire immediately
            int start_rhs = RHS_p[j];
            int end_rhs = RHS_p[j+1];
            for (int k = start_rhs; k < end_rhs; ++k) {
                int attr_rhs = RHS_i[k];
                if (!S.test(attr_rhs)) {
                    S.set(attr_rhs);
                    update.push_back(attr_rhs);
                }
            }
        } else {
            for (int k = start; k < end; ++k) {
                int attr = LHS_i[k];
                if (attr >= 0 && attr < n_attrs) list_m[attr].push_back(j);
            }
        }
    }

    // Add elements initially in S to update
    for (size_t i = S.find_first(); i != Bitset::npos; i = S.find_next(i)) {
        update.push_back(static_cast<int>(i));
    }
    // Remove duplicates
    std::sort(update.begin(), update.end());
    update.erase(std::unique(update.begin(), update.end()), update.end());

    // Standard LINCLOSURE
    while (!update.empty()) {
        int m = update.back();
        update.pop_back();

        if (m < 0 || m >= n_attrs) continue;

        for (int j : list_m[m]) {
            if (--count[j] == 0) {
                int start_rhs = RHS_p[j];
                int end_rhs = RHS_p[j+1];
                for (int k = start_rhs; k < end_rhs; ++k) {
                    int attr_rhs = RHS_i[k];
                    if (!S.test(attr_rhs)) {
                        S.set(attr_rhs);
                        update.push_back(attr_rhs);
                    }
                }
            }
        }
    }

    // Convert result back to Sparse Vector indices (sorted, 1-based for R)
    std::vector<int> res_i;
    for (size_t i = S.find_first(); i != Bitset::npos; i = S.find_next(i)) {
        res_i.push_back(static_cast<int>(i) + 1);
    }

    return List::create(_["i"] = wrap(res_i));
}
