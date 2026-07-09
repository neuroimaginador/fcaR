#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <algorithm>

using namespace Rcpp;

// =============================================================================
// BITSET UTILS (shared with grecond.cpp)
// =============================================================================
using Bitset64 = std::vector<uint64_t>;

inline void set_bit(Bitset64& bs, int index) {
    bs[index / 64] |= (1ULL << (index % 64));
}

inline bool get_bit(const Bitset64& bs, int index) {
    return (bs[index / 64] & (1ULL << (index % 64))) != 0;
}

inline int count_bits(const Bitset64& bs, int n) {
    int sum = 0;
    int w = (n + 63) / 64;
    if (w == 0) return 0;
    for (int i = 0; i < w - 1; i++) sum += __builtin_popcountll(bs[i]);
    uint64_t mask = (n % 64 == 0) ? ~0ULL : (1ULL << (n % 64)) - 1;
    sum += __builtin_popcountll(bs[w - 1] & mask);
    return sum;
}

inline void clean_bitset(Bitset64& bs, int n) {
    int w = (n + 63) / 64;
    if (w == 0) return;
    uint64_t mask = (n % 64 == 0) ? ~0ULL : (1ULL << (n % 64)) - 1;
    bs[w - 1] &= mask;
}

// =============================================================================
// GALOIS CONNECTION — matches MATLAB all() semantics
// extent↑ = intent (columns shared by ALL rows in extent)
// intent↓ = extent (rows shared by ALL columns in intent)
// Empty set → all ones (MATLAB: all([],1) = true)
// =============================================================================

// extent (size wB, indexes over rows) → intent (size wA, indexes over cols)
// R[i] = row i as bitset over columns
void intent_closure(const Bitset64& extent, const std::vector<Bitset64>& R,
                        int m, int n, int wA, int wB, Bitset64& result) {
    std::fill(result.begin(), result.end(), ~0ULL);
    clean_bitset(result, n);
    bool any = false;
    for (int wb = 0; wb < wB; wb++) {
        uint64_t w_ext = extent[wb];
        while (w_ext) {
            int i = wb * 64 + __builtin_ctzll(w_ext);
            for (int w = 0; w < wA; w++) result[w] &= R[i][w];
            any = true; w_ext &= w_ext - 1;
        }
    }
    if (!any) std::fill(result.begin(), result.end(), 0ULL);
}

// intent (size wA, indexes over cols) → extent (size wB, indexes over rows)
// R_T[j] = column j as bitset over rows
void extent_closure(const Bitset64& intent, const std::vector<Bitset64>& R_T,
                        int m, int n, int wA, int wB, Bitset64& result) {
    std::fill(result.begin(), result.end(), ~0ULL);
    clean_bitset(result, m);
    bool any = false;
    for (int wa = 0; wa < wA; wa++) {
        uint64_t w_int = intent[wa];
        while (w_int) {
            int j = wa * 64 + __builtin_ctzll(w_int);
            for (int w = 0; w < wB; w++) result[w] &= R_T[j][w];
            any = true; w_int &= w_int - 1;
        }
    }
    if (!any) std::fill(result.begin(), result.end(), 0ULL);
}

// =============================================================================
// STEP 1: Compute Ess — IDENTICAL to c_ess.c / ess_utils.cpp
//
// (i,j) is essential iff:
//   Row-ess: No row k that is a STRICT SUBSET of row i also has attribute j
//   Col-ess: No col k that is a STRICT SUBSET of col j also has object i
// =============================================================================

void compute_ess(const std::vector<Bitset64>& R,
                 const std::vector<Bitset64>& R_T,
                 int m, int n, int wA, int wB,
                 std::vector<Bitset64>& Ess,
                 std::vector<Bitset64>& Ess_T) {

    Ess.assign(m, Bitset64(wA, 0ULL));
    Ess_T.assign(n, Bitset64(wB, 0ULL));

    std::vector<int> row_weights(m, 0);
    for (int i = 0; i < m; i++) row_weights[i] = count_bits(R[i], n);
    
    std::vector<int> col_weights(n, 0);
    for (int j = 0; j < n; j++) col_weights[j] = count_bits(R_T[j], m);

    // Precompute: row_strict_subset[i] = {ip : row ip ⊂ row i (strict)}
    std::vector<Bitset64> row_ss(m, Bitset64(wB, 0ULL));
    for (int i = 0; i < m; i++) {
        for (int ip = 0; ip < m; ip++) {
            if (i == ip || row_weights[ip] >= row_weights[i]) continue;
            bool subset = true;
            for (int w = 0; w < wA; w++) {
                if ((R[ip][w] & R[i][w]) != R[ip][w]) { subset = false; break; }
            }
            if (subset) set_bit(row_ss[i], ip);
        }
    }

    // col_strict_subset[j] = {jp : col jp ⊂ col j (strict)}
    std::vector<Bitset64> col_ss(n, Bitset64(wA, 0ULL));
    for (int j = 0; j < n; j++) {
        for (int jp = 0; jp < n; jp++) {
            if (j == jp || col_weights[jp] >= col_weights[j]) continue;
            bool subset = true;
            for (int w = 0; w < wB; w++) {
                if ((R_T[jp][w] & R_T[j][w]) != R_T[jp][w]) { subset = false; break; }
            }
            if (subset) set_bit(col_ss[j], jp);
        }
    }

    // Mark essential entries
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            if (!get_bit(R[i], j)) continue;

            // Row-ess: is there any strict subset of row i that has attr j?
            bool ess_r = true;
            for (int w = 0; w < wB; w++) {
                if (row_ss[i][w] & R_T[j][w]) { ess_r = false; break; }
            }
            if (!ess_r) continue;

            // Col-ess: is there any strict subset of col j that has obj i?
            bool ess_s = true;
            for (int w = 0; w < wA; w++) {
                if (col_ss[j][w] & R[i][w]) { ess_s = false; break; }
            }
            if (ess_s) { set_bit(Ess[i], j); set_bit(Ess_T[j], i); }
        }
    }
}

// =============================================================================
// STEP 2: computeIntervals — mirrors MATLAB computeIntervals() exactly
//
// MATLAB stores: A = [A, c]; B = [B; d];  (c = Ess extent, d = Ess intent)
// Removal:  removeB = all(I(a,:),1);  removeA = all(I(:,removeB),2);
//           U(removeA, removeB) = 0;
// Note: removeA uses removeB (NESTED closure), NOT d.
// =============================================================================

struct Interval {
    Bitset64 c_ess; // extent in Ess (logical vector over rows)
    Bitset64 d_ess; // intent in Ess (logical vector over cols)
};

void compute_intervals(const std::vector<Bitset64>& Ess,
                       const std::vector<Bitset64>& Ess_T,
                       const std::vector<Bitset64>& R,
                       const std::vector<Bitset64>& R_T,
                       int m, int n, int wA, int wB,
                       std::vector<Interval>& intervals) {

    std::vector<Bitset64> U_ess = Ess;
    intervals.clear();

    while (true) {
        bool u_empty = true;
        for (int i = 0; i < m && u_empty; i++)
            for (int w = 0; w < wA; w++) if (U_ess[i][w]) u_empty = false;
        if (u_empty) break;

        int v = 0;
        Bitset64 d(wA, 0ULL), d_old(wA, 0ULL), d_mid(wA, 0ULL);
        Bitset64 c_best(wB, 0ULL);
        Bitset64 finalRemA(wB, 0ULL), finalRemB(wA, 0ULL);

        // atr = find(sum(U)>0)
        std::vector<int> atr;
        for (int j = 0; j < n; j++) {
            bool any_u = false;
            for (int i = 0; i < m; i++)
                if (get_bit(U_ess[i], j)) { any_u = true; break; }
            if (any_u) atr.push_back(j);
        }

        Bitset64 a(wB, 0ULL), b(wA, 0ULL), remB(wA, 0ULL), remA(wB, 0ULL);

        while (true) {
            for (int j : atr) {
                if (get_bit(d, j)) continue;

                // d(j) = 1 → compute a = all(Ess(:,d), 2)
                set_bit(d, j);
                extent_closure(d, Ess_T, m, n, wA, wB, a);
                int sum_a = count_bits(a, m);

                if (sum_a * n > v) {
                    // b = all(Ess(a,:), 1)
                    intent_closure(a, Ess, m, n, wA, wB, b);
                    int sum_b = count_bits(b, n);

                    if (sum_a * sum_b > v) {
                        // removeB = all(I(a,:), 1)
                        intent_closure(a, R, m, n, wA, wB, remB);
                        // removeA = all(I(:,removeB), 2) — uses remB, NOT d!
                        extent_closure(remB, R_T, m, n, wA, wB, remA);

                        // area = sum(sum(U(a,b)))
                        int area = 0;
                        for (int wb = 0; wb < wB; wb++) {
                            uint64_t w_a = a[wb];
                            while (w_a) {
                                int i = wb * 64 + __builtin_ctzll(w_a);
                                for (int w = 0; w < wA; w++) area += __builtin_popcountll(U_ess[i][w] & b[w]);
                                w_a &= w_a - 1;
                            }
                        }

                        if (area > v) {
                            v = area;
                            d_mid = b;
                            c_best = a;
                            finalRemA = remA;
                            finalRemB = remB;
                        }
                    }
                }
                // d(j) = 0
                d[j / 64] &= ~(1ULL << (j % 64));
            }

            d = d_mid;
            if (d == d_old) break;
            d_old = d;
        }

        intervals.push_back({c_best, d_mid});

        // U(finalRemoveA, finalRemoveB) = 0
        for (int wb = 0; wb < wB; wb++) {
            uint64_t w_remA = finalRemA[wb];
            while (w_remA) {
                int i = wb * 64 + __builtin_ctzll(w_remA);
                for (int w = 0; w < wA; w++) U_ess[i][w] &= ~finalRemB[w];
                w_remA &= w_remA - 1;
            }
        }
    }
}

// =============================================================================
// STEP 3: get_factor — mirrors MATLAB get_factor() exactly
//
// MATLAB receives submatrix (full-size, zeroed outside interval).
// We simulate it by restricting to remA (rows) and using R directly.
// e = true(m,1); no e-update (MATLAB never updates e in get_factor)
// =============================================================================

void get_factor(const std::vector<Bitset64>& R,
                const std::vector<Bitset64>& U,
                const Bitset64& c_I, const Bitset64& d_I,
                int m, int n, int wA, int wB,
                std::vector<Bitset64>& sub,
                std::vector<Bitset64>& sub_T,
                Bitset64& out_c, Bitset64& out_d) {

    // Clean sub buffers first
    for(int i=0; i<m; i++) std::fill(sub[i].begin(), sub[i].end(), 0ULL);
    for(int j=0; j<n; j++) std::fill(sub_T[j].begin(), sub_T[j].end(), 0ULL);

    // Build submatrix as bitsets: submatrix[i] = (i in c_I) ? R[i] & d_I : 0
    for (int wb = 0; wb < wB; wb++) {
        uint64_t w_c = c_I[wb];
        while (w_c) {
            int i = wb * 64 + __builtin_ctzll(w_c);
            for (int w = 0; w < wA; w++) {
                uint64_t val = R[i][w] & d_I[w];
                sub[i][w] = val;
                uint64_t w_val = val;
                while (w_val) {
                    int j = w * 64 + __builtin_ctzll(w_val);
                    set_bit(sub_T[j], i);
                    w_val &= w_val - 1;
                }
            }
            w_c &= w_c - 1;
        }
    }

    int v = 0;
    Bitset64 d(wA, 0ULL), d_old(wA, 0ULL), d_temp(wA, 0ULL);
    Bitset64 c(wB, 0ULL);
    Bitset64 b(wA, 0ULL);

    // atr = find(sum(submatrix) > 0)
    std::vector<int> atr;
    for (int j = 0; j < n; j++) {
        bool any_s = false;
        for (int w = 0; w < wB; w++) if (sub_T[j][w]) { any_s = true; break; }
        if (any_s) atr.push_back(j);
    }

    while (true) {
        for (int j : atr) {
            if (get_bit(d, j)) continue;
            set_bit(d, j);

            // a = e & submatrix(:,j), but e is all ones
            Bitset64 a = sub_T[j];
            int sum_a = count_bits(a, m);
            
            if (sum_a * n > v) {
                // b = all(submatrix(a,:), 1)
                intent_closure(a, sub, m, n, wA, wB, b);
                
                int sum_b = count_bits(b, n);
                if (sum_a * sum_b > v) {
                    // area = sum(sum(U(a,b)))
                    int area = 0;
                    for (int wb = 0; wb < wB; wb++) {
                        uint64_t w_a = a[wb];
                        while (w_a) {
                            int i = wb * 64 + __builtin_ctzll(w_a);
                            for (int w = 0; w < wA; w++) area += __builtin_popcountll(U[i][w] & b[w]);
                            w_a &= w_a - 1;
                        }
                    }
                    if (area > v) {
                        v = area;
                        d_temp = b;
                        c = a;
                    }
                }
            }
            // d(j) = 0
            d[j / 64] &= ~(1ULL << (j % 64));
        }
        d = d_temp;
        // NO e = c update — matches MATLAB exactly
        if (d == d_old) break;
        d_old = d;
    }
    out_c = c; out_d = d;
}

// =============================================================================
// MAIN: greess_vauthor_cpp — mirrors MATLAB GreEss() main loop exactly
//
// MATLAB:
//   d = all(I(C(:,k),:), 1);   → c_ess↑I
//   c = all(I(:,D(k,:)), 2);   → d_ess↓I
//   submatrix(c, d) = I(c, d)
//   [E, F] = get_factor(submatrix, U)
//   p = sum(sum(U(E,F)))
// =============================================================================

// [[Rcpp::export]]
List greess_cpp(LogicalMatrix I_in) {
    int m = I_in.nrow(), n = I_in.ncol();
    int wA = (n + 63) / 64, wB = (m + 63) / 64;

    std::vector<Bitset64> R(m, Bitset64(wA, 0ULL));
    std::vector<Bitset64> R_T(n, Bitset64(wB, 0ULL));
    for (int i = 0; i < m; i++)
        for (int j = 0; j < n; j++)
            if (I_in(i, j)) { set_bit(R[i], j); set_bit(R_T[j], i); }

    // Step 1: Ess
    std::vector<Bitset64> Ess, Ess_T;
    compute_ess(R, R_T, m, n, wA, wB, Ess, Ess_T);

    // Step 2: Intervals
    std::vector<Interval> intervals;
    compute_intervals(Ess, Ess_T, R, R_T, m, n, wA, wB, intervals);

    // Step 3: Main loop
    std::vector<Bitset64> U = R;
    std::vector<Bitset64> out_A, out_B;
    std::vector<bool> exclusion(intervals.size(), false);
    
    // Submatrix buffers for get_factor zero-allocation
    std::vector<Bitset64> sub_buf(m, Bitset64(wA, 0ULL));
    std::vector<Bitset64> sub_T_buf(n, Bitset64(wB, 0ULL));

    while (true) {
        // Check if U is empty
        bool u_empty = true;
        for (int i = 0; i < m && u_empty; i++)
            for (int w = 0; w < wA; w++) if (U[i][w]) u_empty = false;
        if (u_empty) break;

        int best_area = 0;
        Bitset64 best_E, best_F;
        int sel = -1;

        for (size_t k = 0; k < intervals.size(); k++) {
            Rcpp::checkUserInterrupt();
            if (exclusion[k]) continue;

            // d = all(I(C(:,k),:), 1)  — c_ess↑I
            Bitset64 d_I(wA, 0ULL);
            intent_closure(intervals[k].c_ess, R, m, n, wA, wB, d_I);
            
            // c = all(I(:,D(k,:)), 2)  — d_ess↓I
            Bitset64 c_I(wB, 0ULL);
            extent_closure(intervals[k].d_ess, R_T, m, n, wA, wB, c_I);

            // Lazy Evaluation Pruning
            int ext_size = count_bits(c_I, m);
            int int_size = count_bits(d_I, n);
            if (ext_size * int_size <= best_area) continue;

            // get_factor on submatrix[c_I, d_I]
            Bitset64 E, F;
            get_factor(R, U, c_I, d_I, m, n, wA, wB, sub_buf, sub_T_buf, E, F);

            // p = sum(sum(U(E,F)))
            int p = 0;
            for (int wb = 0; wb < wB; wb++) {
                uint64_t w_E = E[wb];
                while (w_E) {
                    int i = wb * 64 + __builtin_ctzll(w_E);
                    for (int w = 0; w < wA; w++) p += __builtin_popcountll(U[i][w] & F[w]);
                    w_E &= w_E - 1;
                }
            }

            if (p > best_area) {
                best_area = p; best_E = E; best_F = F; sel = k;
            }
        }

        if (sel == -1) break;

        out_A.push_back(best_E);
        out_B.push_back(best_F);
        exclusion[sel] = true;

        // U(vE, vF) = 0
        for (int wb = 0; wb < wB; wb++) {
            uint64_t w_E = best_E[wb];
            while (w_E) {
                int i = wb * 64 + __builtin_ctzll(w_E);
                for (int w = 0; w < wA; w++) U[i][w] &= ~best_F[w];
                w_E &= w_E - 1;
            }
        }
    }

    int K = out_A.size();
    LogicalMatrix A_mat(m, K), B_mat(K, n);
    for (int j = 0; j < K; j++) {
        for (int i = 0; i < m; i++) if (get_bit(out_A[j], i)) A_mat(i, j) = true;
        for (int i = 0; i < n; i++) if (get_bit(out_B[j], i)) B_mat(j, i) = true;
    }
    return List::create(Named("U") = A_mat, Named("V") = B_mat);
}
