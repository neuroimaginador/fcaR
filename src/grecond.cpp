#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <algorithm>

using namespace Rcpp;

// =============================================================================
// BITSET UTILS (Optimized)
// =============================================================================
typedef std::vector<uint64_t> Bitset64;

inline void set_bit(Bitset64& bs, int index) { bs[index >> 6] |= (1ULL << (index & 63)); }
inline bool get_bit(const Bitset64& bs, int index) { return (bs[index >> 6] >> (index & 63)) & 1ULL; }

inline int count_bits(const Bitset64& bs) {
    int c = 0;
    for (uint64_t w : bs) c += __builtin_popcountll(w);
    return c;
}

inline void clean_bitset(Bitset64& bs, int n) {
    if (n == 0) return;
    int w = (n + 63) >> 6;
    int rem = n & 63;
    if (rem != 0) {
        bs[w - 1] &= (1ULL << rem) - 1;
    }
}

// =============================================================================
// GRECOND v2 (Zero-Allocation + Pruning + Attribute Filtering)
// =============================================================================

// [[Rcpp::export]]
List grecond_cpp(LogicalMatrix I, int no_of_factors = -1) {
    int m = I.nrow(), n = I.ncol();
    int wA = (n + 63) >> 6, wB = (m + 63) >> 6;

    // Precompute Columns and Rows as Bitsets
    std::vector<Bitset64> Rows(m, Bitset64(wA, 0ULL));
    std::vector<Bitset64> Cols(n, Bitset64(wB, 0ULL));
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            if (I(i, j)) {
                set_bit(Rows[i], j);
                set_bit(Cols[j], i);
            }
        }
    }

    std::vector<Bitset64> U = Rows;
    std::vector<Bitset64> out_A, out_B;
    int k = 0;

    // Reusable buffers to avoid allocations in the loops
    Bitset64 next_A(wB, 0ULL), next_B(wA, 0ULL);
    Bitset64 next_A_best(wB, 0ULL), next_B_best(wA, 0ULL);
    Bitset64 curr_A(wB, 0ULL), curr_B(wA, 0ULL);
    Bitset64 empty_A(wB, 0ULL), empty_B(wA, 0ULL);
    Bitset64 full_A(wB, ~0ULL); clean_bitset(full_A, m);

    while (true) {
        // Find attributes with coverage > 0 in residue U
        std::vector<int> atr;
        for (int j = 0; j < n; j++) {
            bool any = false;
            for (int i = 0; i < m; i++) {
                if (get_bit(Cols[j], i)) {
                    if (get_bit(U[i], j)) { any = true; break; }
                }
                if (any) break;
            }
            if (any) atr.push_back(j);
        }
        if (atr.empty()) break;

        int best_v = 0;
        curr_A = full_A;
        std::fill(curr_B.begin(), curr_B.end(), 0ULL);

        while (true) {
            int local_best_v = best_v;
            int best_attr = -1;

            for (int j : atr) {
                if (get_bit(curr_B, j)) continue;

                // Step 1: Extent update (bitwise AND)
                next_A = curr_A;
                for (int w = 0; w < wB; w++) next_A[w] &= Cols[j][w];
                int sz_A = count_bits(next_A);

                // Pruning 1: Max Area
                if (sz_A * n <= local_best_v) continue;

                // Step 2: Intent update (bitwise AND rows)
                std::fill(next_B.begin(), next_B.end(), ~0ULL);
                bool has_obj = false;
                for (int i = 0; i < m; i++) {
                    if (get_bit(next_A, i)) {
                        for (int w = 0; w < wA; w++) next_B[w] &= Rows[i][w];
                        has_obj = true;
                    }
                }
                if (!has_obj) std::fill(next_B.begin(), next_B.end(), 0ULL);
                else clean_bitset(next_B, n);

                int sz_B = count_bits(next_B);
                // Pruning 2: Concept Area
                if (sz_A * sz_B <= local_best_v) continue;

                // Step 3: Coverage calculation
                int cov = 0;
                for (int i = 0; i < m; i++) {
                    if (get_bit(next_A, i)) {
                        for (int w = 0; w < wA; w++) cov += __builtin_popcountll(U[i][w] & next_B[w]);
                    }
                }

                if (cov > local_best_v) {
                    local_best_v = cov;
                    best_attr = j;
                    next_A_best = next_A;
                    next_B_best = next_B;
                }
            }

            if (best_attr == -1 || local_best_v <= best_v) break;

            best_v = local_best_v;
            curr_A = next_A_best;
            curr_B = next_B_best;
        }

        out_A.push_back(curr_A);
        out_B.push_back(curr_B);
        k++;

        // Update residue U
        for (int i = 0; i < m; i++) {
            if (get_bit(curr_A, i)) {
                for (int w = 0; w < wA; w++) U[i][w] &= ~curr_B[w];
            }
        }
        if (no_of_factors > 0 && k == no_of_factors) break;
    }

    LogicalMatrix A_mat(m, k), B_mat(k, n);
    for (int j = 0; j < k; j++) {
        for (int i = 0; i < m; i++) if (get_bit(out_A[j], i)) A_mat(i, j) = true;
        for (int i = 0; i < n; i++) if (get_bit(out_B[j], i)) B_mat(j, i) = true;
    }
    return List::create(Named("U") = A_mat, Named("V") = B_mat);
}
