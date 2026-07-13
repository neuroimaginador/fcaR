#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

using Bitset64 = std::vector<uint64_t>;

inline void set_bit(Bitset64& bs, int index) {
    bs[index / 64] |= (1ULL << (index % 64));
}

inline bool get_bit(const Bitset64& bs, int index) {
    return (bs[index / 64] & (1ULL << (index % 64))) != 0;
}

inline void clear_bit(Bitset64& bs, int index) {
    bs[index / 64] &= ~(1ULL << (index % 64));
}

inline int count_bits(const Bitset64& bs) {
    int sum = 0;
    for (uint64_t val : bs) sum += __builtin_popcountll(val);
    return sum;
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

// Optimized PaNDa+
// [[Rcpp::export]]
List panda_plus_unified_cpp(LogicalMatrix I_in, int k_max, std::string cost_func = "J_P", double rho = 1.0) {
    int m = I_in.nrow();
    int n = I_in.ncol();
    int w = (n + 63) / 64;
    int w_rows = (m + 63) / 64;

    std::vector<Bitset64> I = matrix_to_bits(I_in);
    std::vector<Bitset64> Rec(m, Bitset64(w, 0ULL));

    std::vector<Bitset64> P_ext;
    std::vector<Bitset64> P_int;

    // c_type: 0 for J_P, 1 for J_A, 2 for J_P_rho
    int c_type = 0;
    if (cost_func == "J_A") c_type = 1;
    else if (cost_func == "J_P_rho") c_type = 2;

    for (int k = 0; k < k_max; ++k) {
        Rcpp::checkUserInterrupt();

        std::vector<std::pair<int, int>> noise_per_col(n);
        for (int j = 0; j < n; j++) {
            int noise = 0;
            for (int i = 0; i < m; i++) {
                if (get_bit(I[i], j) && !get_bit(Rec[i], j)) noise++; 
            }
            noise_per_col[j] = {noise, j};
        }
        std::sort(noise_per_col.begin(), noise_per_col.end(), [](const std::pair<int, int>& a, const std::pair<int, int>& b) { return a.first > b.first; });

        if (noise_per_col[0].first == 0) break; 

        // find_core (PyBMF logic)
        Bitset64 C_I(w, 0ULL);
        Bitset64 C_T(w_rows, 0ULL);
        int top_col = noise_per_col[0].second;
        set_bit(C_I, top_col);
        for (int i = 0; i < m; i++) if (get_bit(I[i], top_col) && !get_bit(Rec[i], top_col)) set_bit(C_T, i);

        double w_model = (c_type == 1) ? 0.0 : ((c_type == 2) ? rho : 1.0);
        
        // Add more columns to core if it decreases cost
        for (int idx = 1; idx < n; idx++) {
            int j = noise_per_col[idx].second;
            Bitset64 T_new(w_rows, 0ULL);
            int new_h = 0;
            for (int i = 0; i < m; i++) {
                if (get_bit(C_T, i) && get_bit(I[i], j) && !get_bit(Rec[i], j)) {
                    set_bit(T_new, i);
                    new_h++;
                }
            }
            
            int w0 = count_bits(C_I), h0 = count_bits(C_T);
            int w1 = w0 + 1, h1 = new_h;
            double d_cost = w_model * ((w1 + h1) - (w0 + h0)) - 1.0 * ((w1 * h1) - (w0 * h0));
            if (d_cost <= 0) {
                set_bit(C_I, j);
                C_T = T_new;
            }
        }

        // extend_core (PyBMF logic)
        // Add more items to I if it decreases cost (using relaxed T intersection)
        for (int idx = 1; idx < n; idx++) {
            int j = noise_per_col[idx].second;
            if (get_bit(C_I, j)) continue;

            double d_fn = 0;
            double d_fp = 0;
            for (int i = 0; i < m; i++) {
                if (get_bit(C_T, i) && !get_bit(Rec[i], j)) {
                    if (get_bit(I[i], j)) d_fn -= 1.0;
                    else d_fp += 1.0;
                }
            }
            double d_cost = w_model * 1.0 + (1.0 * d_fn + 1.0 * d_fp);
            if (d_cost <= 0) {
                set_bit(C_I, j);
            }
        }

        // Add more transactions to T if it decreases cost
        for (int i = 0; i < m; i++) {
            if (get_bit(C_T, i)) continue;

            double d_fn = 0, d_fp = 0;
            for (int j_idx = 0; j_idx < w; j_idx++) {
                uint64_t uncovered = C_I[j_idx] & ~Rec[i][j_idx];
                d_fn -= __builtin_popcountll(uncovered & I[i][j_idx]);
                d_fp += __builtin_popcountll(uncovered & ~I[i][j_idx]);
            }
            double d_cost = w_model * 1.0 + (1.0 * d_fn + 1.0 * d_fp);
            if (d_cost <= 0) {
                set_bit(C_T, i);
            }
        }

        int ct_size = count_bits(C_T);
        int ci_size = count_bits(C_I);
        if (ct_size == 0 || ci_size == 0) break;

        // Verify total cost reduction relative to an empty factor
        double delta_factors_total = ct_size + ci_size;
        double delta_error_total = 0;
        for (int i = 0; i < m; i++) {
            if (get_bit(C_T, i)) {
                for (int j_idx = 0; j_idx < w; j_idx++) {
                    uint64_t uncovered = C_I[j_idx] & ~Rec[i][j_idx];
                    delta_error_total += __builtin_popcountll(uncovered & ~I[i][j_idx]);
                    delta_error_total -= __builtin_popcountll(uncovered & I[i][j_idx]);
                }
            }
        }

        double delta_cost = 0;
        if (c_type == 0) delta_cost = delta_factors_total + delta_error_total;
        else if (c_type == 1) delta_cost = delta_error_total;
        else if (c_type == 2) delta_cost = (rho * delta_factors_total) + delta_error_total;

        if (delta_cost < 0) {
            P_ext.push_back(C_T);
            P_int.push_back(C_I);
            for (int i = 0; i < m; i++) {
                if (get_bit(C_T, i)) {
                    for (int j_idx = 0; j_idx < w; j_idx++) Rec[i][j_idx] |= C_I[j_idx];
                }
            }
        } else {
            break;
        }
    }

    int K = P_ext.size();
    LogicalMatrix U_mat(m, K), V_mat(K, n);
    for (int k_idx = 0; k_idx < K; ++k_idx) {
        for (int i = 0; i < m; i++) if (get_bit(P_ext[k_idx], i)) U_mat(i, k_idx) = true;
        for (int j = 0; j < n; j++) if (get_bit(P_int[k_idx], j)) V_mat(k_idx, j) = true;
    }
    return List::create(Named("U") = U_mat, Named("V") = V_mat);
}

// [[Rcpp::export]]
List panda_plus_jp_cpp(LogicalMatrix I_in, int k_max) { return panda_plus_unified_cpp(I_in, k_max, "J_P", 1.0); }

// [[Rcpp::export]]
List panda_plus_ja_cpp(LogicalMatrix I_in, int k_max) { return panda_plus_unified_cpp(I_in, k_max, "J_A", 1.0); }

// [[Rcpp::export]]
List panda_plus_jprho_cpp(LogicalMatrix I_in, int k_max, double rho) { return panda_plus_unified_cpp(I_in, k_max, "J_P_rho", rho); }
