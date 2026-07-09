#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <cmath>

using namespace Rcpp;

using Bitset64 = std::vector<uint64_t>;

inline void set_bit(Bitset64& bs, int index) {
    bs[index / 64] |= (1ULL << (index % 64));
}

inline bool get_bit(const Bitset64& bs, int index) {
    return (bs[index / 64] & (1ULL << (index % 64))) != 0;
}

inline std::vector<Bitset64> matrix_to_bits(LogicalMatrix I) {
    int m = I.nrow();
    int n = I.ncol();
    int w = (n + 63) / 64;
    std::vector<Bitset64> R(m, Bitset64(w, 0ULL));
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            if (I(i, j)) set_bit(R[i], j);
        }
    }
    return R;
}

// Optimized ASSO
// [[Rcpp::export]]
List asso_bitwise_cpp(LogicalMatrix I_in, int k_max, double threshold, double w_pos, double w_neg) {
    int m = I_in.nrow(), n = I_in.ncol();
    int w = (n + 63) / 64;
    int w_rows = (m + 63) / 64;
    
    std::vector<Bitset64> I = matrix_to_bits(I_in);
    std::vector<Bitset64> I_T(n, Bitset64(w_rows, 0ULL));
    for(int i=0; i<m; i++) for(int j=0; j<n; j++) if(I_in(i, j)) set_bit(I_T[j], i);

    std::vector<Bitset64> A(n, Bitset64(w, 0ULL));
    for (int i = 0; i < n; ++i) {
        int supp_i = 0;
        for (int k = 0; k < w_rows; k++) supp_i += __builtin_popcountll(I_T[i][k]);
        if (supp_i == 0) continue;
        for (int j = 0; j < n; ++j) {
            int inter = 0;
            for (int k = 0; k < w_rows; k++) inter += __builtin_popcountll(I_T[i][k] & I_T[j][k]);
            if ((double)inter / supp_i >= threshold) set_bit(A[i], j);
        }
    }

    std::vector<Bitset64> Rec(m, Bitset64(w, 0ULL));
    std::vector<double> cached_gains(n, 0.0);
    std::vector<Bitset64> cached_S(n, Bitset64(w_rows, 0ULL));
    std::vector<bool> active_candidates(n, true);

    // Initial cache population
    for (int i = 0; i < n; ++i) {
        double gain = 0;
        for (int r = 0; r < m; ++r) {
            double row_gain = 0;
            for (int w_idx = 0; w_idx < w; ++w_idx) {
                uint64_t pos_bits = A[i][w_idx] & I[r][w_idx];
                uint64_t neg_bits = A[i][w_idx] & ~I[r][w_idx];
                row_gain += __builtin_popcountll(pos_bits) * w_pos;
                row_gain -= __builtin_popcountll(neg_bits) * w_neg;
            }
            if (row_gain > 0) {
                set_bit(cached_S[i], r);
                gain += row_gain;
            }
        }
        cached_gains[i] = gain;
    }

    std::vector<Bitset64> out_ext, out_int;

    for (int k = 0; k < k_max; ++k) {
        Rcpp::checkUserInterrupt();
        
        int best_i = -1;
        double max_gain = 0;
        for (int i = 0; i < n; ++i) {
            if (active_candidates[i] && cached_gains[i] > max_gain) {
                max_gain = cached_gains[i];
                best_i = i;
            }
        }

        if (best_i == -1 || max_gain <= 0) break;

        active_candidates[best_i] = false;
        Bitset64 best_B = A[best_i];
        Bitset64 best_S = cached_S[best_i];

        out_ext.push_back(best_S);
        out_int.push_back(best_B);

        // Update Rec and delta-update cached gains
        for (int r = 0; r < m; ++r) {
            if (get_bit(best_S, r)) {
                // Find bits that JUST flipped to 1 in Rec
                Bitset64 newly_covered(w, 0ULL);
                for (int w_idx = 0; w_idx < w; ++w_idx) {
                    newly_covered[w_idx] = best_B[w_idx] & ~Rec[r][w_idx];
                    Rec[r][w_idx] |= newly_covered[w_idx];
                }

                // If no new bits covered in this row, skip
                bool changed = false;
                for(int w_idx=0; w_idx<w; ++w_idx) if(newly_covered[w_idx]) { changed = true; break; }
                if(!changed) continue;

                for (int i = 0; i < n; ++i) {
                    if (!active_candidates[i]) continue;

                    // Check overlap between candidate intent and newly covered bits
                    bool overlaps = false;
                    for (int w_idx = 0; w_idx < w; ++w_idx) {
                        if (A[i][w_idx] & newly_covered[w_idx]) { overlaps = true; break; }
                    }
                    if (!overlaps) continue;

                    // Re-evaluate candidate i on row r
                    // Old row gain
                    double old_row_gain = 0;
                    if (get_bit(cached_S[i], r)) {
                        for (int w_idx = 0; w_idx < w; ++w_idx) {
                            uint64_t uncovered = A[i][w_idx] & ~(Rec[r][w_idx] & ~newly_covered[w_idx]); // Rec BEFORE update
                            old_row_gain += __builtin_popcountll(uncovered & I[r][w_idx]) * w_pos;
                            old_row_gain -= __builtin_popcountll(uncovered & ~I[r][w_idx]) * w_neg;
                        }
                    }

                    // New row gain
                    double new_row_gain = 0;
                    for (int w_idx = 0; w_idx < w; ++w_idx) {
                        uint64_t uncovered = A[i][w_idx] & ~Rec[r][w_idx]; // Rec AFTER update
                        new_row_gain += __builtin_popcountll(uncovered & I[r][w_idx]) * w_pos;
                        new_row_gain -= __builtin_popcountll(uncovered & ~I[r][w_idx]) * w_neg;
                    }

                    // Update S and Gain
                    if (get_bit(cached_S[i], r)) {
                        if (new_row_gain > 0) {
                            cached_gains[i] += (new_row_gain - old_row_gain);
                        } else {
                            cached_S[i][r/64] &= ~(1ULL << (r%64));
                            cached_gains[i] -= old_row_gain;
                        }
                    } else {
                        if (new_row_gain > 0) {
                            set_bit(cached_S[i], r);
                            cached_gains[i] += new_row_gain;
                        }
                    }
                }
            }
        }
    }

    int K = out_ext.size();
    LogicalMatrix U_mat(m, K), V_mat(K, n);
    for (int k_idx = 0; k_idx < K; ++k_idx) {
        for (int i = 0; i < m; i++) if (get_bit(out_ext[k_idx], i)) U_mat(i, k_idx) = true;
        for (int j = 0; j < n; j++) if (get_bit(out_int[k_idx], j)) V_mat(k_idx, j) = true;
    }
    return List::create(Named("U") = U_mat, Named("V") = V_mat);
}
