#include <Rcpp.h>
#include <vector>
#include <chrono>
#include "fcaR_bitset.h"

using namespace Rcpp;
using namespace fcaR; // FastBitset

// Optimized LinClosure for FastBitset
void fast_lin_closure(FastBitset& A, const std::vector<FastBitset>& lhs_list, const std::vector<FastBitset>& rhs_list) {
    bool changed = true;
    while (changed) {
        changed = false;
        for (size_t i = 0; i < lhs_list.size(); ++i) {
            if (lhs_list[i].is_subset_of(A)) {
                if (!rhs_list[i].is_subset_of(A)) {
                    A |= rhs_list[i];
                    changed = true;
                }
            }
        }
    }
}

// Bitset-based NextClosure for Implications
void get_closed_sets_bitset(const std::vector<FastBitset>& lhs_list, 
                           const std::vector<FastBitset>& rhs_list, 
                           int n_attrs, 
                           std::vector<FastBitset>& out_concepts) {
    FastBitset A;
    A.init(n_attrs);
    
    // Initial closure of empty set
    fast_lin_closure(A, lhs_list, rhs_list);
    out_concepts.push_back(A);

    FastBitset candidate;
    candidate.init(n_attrs);

    while (A.count() < n_attrs) {
        bool success = false;
        for (int i = n_attrs - 1; i >= 0; --i) {
            if (A.test(i)) continue;

            // candidate = (A \cap {0..i-1} \cup {i})^\uparrow
            candidate.clear();
            for (int j = 0; j < i; ++j) if (A.test(j)) candidate.set(j);
            candidate.set(i);
            
            fast_lin_closure(candidate, lhs_list, rhs_list);

            // Canonicity test
            bool is_canonical = true;
            for (int j = 0; j < i; ++j) {
                if (candidate.test(j) != A.test(j)) {
                    is_canonical = false;
                    break;
                }
            }

            if (is_canonical) {
                A = candidate;
                out_concepts.push_back(A);
                success = true;
                break;
            }
        }
        if (!success) break;
    }
}

// Internal implementation of binary_next_closure_implications returning bitsets
void compute_imps_bitset(IntegerMatrix I, std::vector<FastBitset>& lhs_out, std::vector<FastBitset>& rhs_out) {
    // This is a simplified version of binary_next_closure_implications using bitsets internally
    int n_obj = I.nrow();
    int n_attr = I.ncol();

    std::vector<FastBitset> objects_by_attr(n_attr);
    std::vector<FastBitset> attributes_by_obj(n_obj);

    for (int j = 0; j < n_attr; ++j) {
        objects_by_attr[j].init(n_obj);
        for (int i = 0; i < n_obj; ++i) if (I(i, j) != 0) objects_by_attr[j].set(i);
    }
    for (int i = 0; i < n_obj; ++i) {
        attributes_by_obj[i].init(n_attr);
        for (int j = 0; j < n_attr; ++j) if (I(i, j) != 0) attributes_by_obj[i].set(j);
    }

    auto compute_closure = [&](const FastBitset& A, FastBitset& out) {
        FastBitset extent;
        extent.init(n_obj);
        extent.set();
        for (int j = 0; j < n_attr; ++j) {
            if (A.test(j)) extent &= objects_by_attr[j];
        }
        out.init(n_attr);
        out.set();
        for (int i = 0; i < n_obj; ++i) {
            if (extent.test(i)) out &= attributes_by_obj[i];
        }
    };

    std::vector<FastBitset> basis_lhs;
    std::vector<FastBitset> basis_rhs;
    
    auto lin_closure_local = [&](FastBitset& A) {
        bool changed = true;
        while (changed) {
            changed = false;
            for (size_t k = 0; k < basis_lhs.size(); ++k) {
                if (basis_lhs[k].is_subset_of(A)) {
                    if (!basis_rhs[k].is_subset_of(A)) {
                        A |= basis_rhs[k];
                        changed = true;
                    }
                }
            }
        }
    };

    FastBitset A;
    A.init(n_attr);
    lin_closure_local(A);

    while (true) {
        FastBitset closure;
        compute_closure(A, closure);

        if (A != closure) {
            basis_lhs.push_back(A);
            FastBitset rhs = closure;
            rhs.bitwise_and_not(A);
            basis_rhs.push_back(rhs);
        }

        bool success = false;
        for (int i = n_attr - 1; i >= 0; --i) {
            if (A.test(i)) {
                A.reset(i);
            } else {
                FastBitset cand = A;
                cand.set(i);
                lin_closure_local(cand);
                
                // equal_prefix check
                bool ok = true;
                for (int j = 0; j < i; ++j) if (cand.test(j) != A.test(j)) { ok = false; break; }
                
                if (ok) {
                    A = cand;
                    success = true;
                    break;
                }
            }
        }
        if (!success) break;
    }
    lhs_out = basis_lhs;
    rhs_out = basis_rhs;
}

// [[Rcpp::export]]
List bonds_standard_opt_cpp(IntegerMatrix I1, IntegerMatrix I2, bool verbose = false) {
    auto start_time = std::chrono::high_resolution_clock::now();

    int g1 = I1.nrow();
    int m1 = I1.ncol();
    int g2 = I2.nrow();
    int m2 = I2.ncol();

    // 1. Dual of I1
    IntegerMatrix I1d(m1, g1);
    for (int i = 0; i < g1; ++i) for (int j = 0; j < m1; ++j) I1d(j, i) = I1(i, j);

    // 2. Implications for I1d and I2
    std::vector<FastBitset> lhs1, rhs1, lhs2, rhs2;
    compute_imps_bitset(I1d, lhs1, rhs1);
    compute_imps_bitset(I2, lhs2, rhs2);

    // 3. Merging implications to cross-context (G1 x M2)
    int n_cross = g1 * m2;
    std::vector<FastBitset> merged_lhs, merged_rhs;

    // From imps1: for each m in M2, (A, B) -> (A x {m}, B x {m})
    for (int m = 0; m < m2; ++m) {
        for (size_t i = 0; i < lhs1.size(); ++i) {
            FastBitset ml, mr;
            ml.init(n_cross);
            mr.init(n_cross);
            for (int j = 0; j < g1; ++j) {
                if (lhs1[i].test(j)) ml.set(m * g1 + j);
                if (rhs1[i].test(j)) mr.set(m * g1 + j);
            }
            if (ml.any() || mr.any()) {
                merged_lhs.push_back(ml);
                merged_rhs.push_back(mr);
            }
        }
    }

    // From imps2: for each g in G1, (A, B) -> ({g} x A, {g} x B)
    for (int g = 0; g < g1; ++g) {
        for (size_t i = 0; i < lhs2.size(); ++i) {
            FastBitset ml, mr;
            ml.init(n_cross);
            mr.init(n_cross);
            for (int j = 0; j < m2; ++j) {
                if (lhs2[i].test(j)) ml.set(j * g1 + g);
                if (rhs2[i].test(j)) mr.set(j * g1 + g);
            }
            if (ml.any() || mr.any()) {
                merged_lhs.push_back(ml);
                merged_rhs.push_back(mr);
            }
        }
    }

    // 4. Find all closed sets of the merged implications
    std::vector<FastBitset> out_intents;
    get_closed_sets_bitset(merged_lhs, merged_rhs, n_cross, out_intents);

    // 5. Format output
    std::vector<int> i_ext, p_ext;
    p_ext.push_back(0);
    for (const auto& b : out_intents) {
        for (int x = 0; x < n_cross; ++x) {
            if (b.test(x)) i_ext.push_back(x);
        }
        p_ext.push_back(static_cast<int>(i_ext.size()));
    }
    
    S4 bonds_mat("dgCMatrix");
    bonds_mat.slot("i") = wrap(i_ext);
    bonds_mat.slot("p") = wrap(p_ext);
    bonds_mat.slot("x") = NumericVector(i_ext.size(), 1.0);
    bonds_mat.slot("Dim") = IntegerVector::create(n_cross, out_intents.size());

    auto end_time = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end_time - start_time;

    return List::create(
        _["intents"] = bonds_mat,
        _["elapsed"] = elapsed.count()
    );
}
