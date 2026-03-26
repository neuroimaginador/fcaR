#include <Rcpp.h>
#include <vector>
#include "fcaR_bitset.h"

using namespace Rcpp;
using namespace fcaR;

// [[Rcpp::export]]
IntegerMatrix bonds_closure_cpp(IntegerMatrix J_in, IntegerMatrix I_in) {
    int n_obj = I_in.nrow();
    int n_attr = I_in.ncol();
    
    // 1. Prepare bitsets for the Formal Context I
    // We need both rows and columns for closures
    std::vector<FastBitset> I_rows(n_obj);
    std::vector<FastBitset> I_cols(n_attr);
    
    for (int i = 0; i < n_obj; ++i) {
        I_rows[i].init(n_attr);
        for (int j = 0; j < n_attr; ++j) if (I_in(i, j) != 0) I_rows[i].set(j);
    }
    for (int j = 0; j < n_attr; ++j) {
        I_cols[j].init(n_obj);
        for (int i = 0; i < n_obj; ++i) if (I_in(i, j) != 0) I_cols[j].set(i);
    }
    
    // 2. Initial J matrix as bitsets
    std::vector<FastBitset> J_rows(n_obj);
    for (int i = 0; i < n_obj; ++i) {
        J_rows[i].init(n_attr);
        for (int j = 0; j < n_attr; ++j) if (J_in(i, j) != 0) J_rows[i].set(j);
    }
    
    // Temporary bitsets for reuse
    FastBitset extent; extent.init(n_obj);
    FastBitset closure; closure.init(n_attr);
    FastBitset intent; intent.init(n_attr);
    FastBitset dual_closure; dual_closure.init(n_obj);
    
    bool changed = true;
    int iter = 0;
    while (changed && iter < 10000) {
        changed = false;
        iter++;
        
        // --- 1. Row Closure ---
        for (int i = 0; i < n_obj; ++i) {
            FastBitset& S = J_rows[i];
            if (S.none()) {
                closure.set();
                for (int ii = 0; ii < n_obj; ++ii) closure &= I_rows[ii];
            } else {
                extent.set();
                for (int j = 0; j < n_attr; ++j) {
                    if (S.test(j)) extent &= I_cols[j];
                }
                closure.set();
                for (int ii = 0; ii < n_obj; ++ii) {
                    if (extent.test(ii)) closure &= I_rows[ii];
                }
            }
            
            if (S != closure) {
                S = closure;
                changed = true;
            }
        }
        
        // --- 2. Col Closure ---
        std::vector<FastBitset> J_cols(n_attr);
        for (int j = 0; j < n_attr; ++j) {
            J_cols[j].init(n_obj);
            for (int i = 0; i < n_obj; ++i) if (J_rows[i].test(j)) J_cols[j].set(i);
        }
        
        for (int j = 0; j < n_attr; ++j) {
            FastBitset& S_dual = J_cols[j];
            if (S_dual.none()) {
                dual_closure.set();
                for (int jj = 0; jj < n_attr; ++jj) dual_closure &= I_cols[jj];
            } else {
                intent.set();
                for (int i = 0; i < n_obj; ++i) {
                    if (S_dual.test(i)) intent &= I_rows[i];
                }
                dual_closure.set();
                for (int jj = 0; jj < n_attr; ++jj) {
                    if (intent.test(jj)) dual_closure &= I_cols[jj];
                }
            }
            
            if (S_dual != dual_closure) {
                for (int i = 0; i < n_obj; ++i) {
                    if (dual_closure.test(i)) J_rows[i].set(j);
                    else J_rows[i].reset(j);
                }
                changed = true;
            }
        }
    }
    
    if (iter >= 1000) {
        Rcout << "Warning: bonds_closure_cpp reached max iterations (1000)." << std::endl;
    }
    
    // 3. Construct Output Matrix
    IntegerMatrix J_out(n_obj, n_attr);
    for (int i = 0; i < n_obj; ++i) {
        for (int j = 0; j < n_attr; ++j) {
            if (J_rows[i].test(j)) J_out(i, j) = 1;
            else J_out(i, j) = 0;
        }
    }
    
    return J_out;
}
