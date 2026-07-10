#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <chrono>


typedef std::vector<uint64_t> Bitset64;

inline void set_bit(std::vector<uint64_t>& bs, int pos, int w_size, int row) { 
    bs[row * w_size + (pos >> 6)] |= (1ULL << (pos & 63)); 
}
inline bool get_bit(const std::vector<uint64_t>& bs, int pos, int w_size, int row) { 
    return (bs[row * w_size + (pos >> 6)] >> (pos & 63)) & 1ULL; 
}
inline void set_bit(Bitset64& bs, int pos) { bs[pos >> 6] |= (1ULL << (pos & 63)); }
inline bool get_bit(const Bitset64& bs, int pos) { return (bs[pos >> 6] >> (pos & 63)) & 1ULL; }
inline int count_bits(const Bitset64& bs) {
    int c = 0; for (uint64_t w : bs) c += __builtin_popcountll(w); return c;
}

// Swapped arrow functions for attribute-based logic
inline void arrow_up(const Bitset64& objects, const std::vector<uint64_t>& R_fast_flat, int wA, int wB, int A, Bitset64& result) {
    std::fill(result.begin(), result.end(), ~0ULL);
    bool any = false;
    for (int wb = 0; wb < wB; wb++) {
        uint64_t w = objects[wb];
        while (w) {
            int b = wb * 64 + __builtin_ctzll(w);
            const uint64_t* r_ptr = &R_fast_flat[b * wA];
            for (int wa = 0; wa < wA; wa++) result[wa] &= r_ptr[wa];
            any = true; w &= w - 1;
        }
    }
    if (!any) std::fill(result.begin(), result.end(), 0ULL);
    else { int rem = A & 63; if (rem > 0) result[wA - 1] &= ((1ULL << rem) - 1); }
}

inline void arrow_down(const Bitset64& attrs, const std::vector<uint64_t>& R_fast_T_flat, int wA, int wB, int B, Bitset64& result) {
    std::fill(result.begin(), result.end(), ~0ULL);
    bool any = false;
    for (int wa = 0; wa < wA; wa++) {
        uint64_t w = attrs[wa];
        while (w) {
            int a = wa * 64 + __builtin_ctzll(w);
            const uint64_t* r_ptr = &R_fast_T_flat[a * wB];
            for (int wb = 0; wb < wB; wb++) result[wb] &= r_ptr[wb];
            any = true; w &= w - 1;
        }
    }
    if (!any) std::fill(result.begin(), result.end(), 0ULL);
    else { int rem = B & 63; if (rem > 0) result[wB - 1] &= ((1ULL << rem) - 1); }
}

struct Concept {
    Bitset64 extent, intent;
    int coverage, extent_size, intent_size, base_id;
    Concept(int wB, int wA) : extent(wB, 0ULL), intent(wA, 0ULL), coverage(0), extent_size(0), intent_size(0), base_id(-1) {}
};

inline int compute_coverage(const Bitset64& X, const Bitset64& Y, const std::vector<uint64_t>& R_un_T_flat, int wA, int wB) {
    int cov = 0;
    for (int wa = 0; wa < wA; wa++) {
        uint64_t y = Y[wa];
        while (y) {
            int a = wa * 64 + __builtin_ctzll(y);
            const uint64_t* row_ptr = &R_un_T_flat[a * wB];
            for (int wb = 0; wb < wB; wb++) cov += __builtin_popcountll(X[wb] & row_ptr[wb]);
            y &= y - 1;
        }
    }
    return cov;
}

inline double compute_rho(const Concept* c1, const Concept* c2, const std::vector<uint64_t>& R_un_T_flat, int wA, int wB, Bitset64& x_inter_buf) {
    int y_inter_size = 0, x_inter_size = 0, num = 0;
    bool has_y = false, has_x = false;

    for (int w = 0; w < wA; w++) {
        uint64_t inter = c1->intent[w] & c2->intent[w];
        if (inter) { has_y = true; y_inter_size += __builtin_popcountll(inter); }
    }
    if (!has_y) return 1.0;

    for (int w = 0; w < wB; w++) {
        uint64_t inter = c1->extent[w] & c2->extent[w];
        x_inter_buf[w] = inter;
        if (inter) { has_x = true; x_inter_size += __builtin_popcountll(inter); }
    }
    if (!has_x) return 1.0;

    for (int wa = 0; wa < wA; wa++) {
        uint64_t y_inter = c1->intent[wa] & c2->intent[wa];
        while (y_inter) {
            int a = wa * 64 + __builtin_ctzll(y_inter);
            const uint64_t* row_ptr = &R_un_T_flat[a * wB];
            for (int wb = 0; wb < wB; wb++) {
                num += __builtin_popcountll(x_inter_buf[wb] & row_ptr[wb]);
            }
            y_inter &= y_inter - 1;
        }
    }
    if (num == 0) return 1.0;

    int den = (c1->extent_size * c1->intent_size) + (c2->extent_size * c2->intent_size) - (x_inter_size * y_inter_size);
    return (den <= 0) ? 1.0 : (1.0 - (double)num / den);
}

// [[Rcpp::export]]
Rcpp::List rsf_attr_cpp(Rcpp::LogicalMatrix R) {
    auto _start_time = std::chrono::high_resolution_clock::now();
    // Work on transposed matrix for attribute-based RSF
    int B_orig = R.nrow(), A_orig = R.ncol();
    int B = A_orig, A = B_orig; // Transposed dimensions
    int wA = (A + 63) / 64, wB = (B + 63) / 64;

    std::vector<uint64_t> R_fast_flat(B * wA, 0ULL);
    std::vector<uint64_t> R_fast_T_flat(A * wB, 0ULL);
    std::vector<uint64_t> R_un_T_flat(A * wB, 0ULL);
    long total_ones = 0;

    for (int i = 0; i < B_orig; i++) {
        for (int j = 0; j < A_orig; j++) {
            if (R(i, j)) {
                // Set in transposed order
                set_bit(R_fast_flat, i, wA, j);
                set_bit(R_fast_T_flat, j, wB, i);
                set_bit(R_un_T_flat, j, wB, i);
                total_ones++;
            }
        }
    }

    std::vector<Concept> C1_base;
    C1_base.reserve(B);
    Bitset64 obj(wB, 0ULL), Y(wA, 0ULL);
    
    for (int b = 0; b < B; b++) {
        std::fill(obj.begin(), obj.end(), 0ULL); obj[b >> 6] |= (1ULL << (b & 63));
        arrow_up(obj, R_fast_flat, wA, wB, A, Y);
        bool dup = false;
        for (const auto& c : C1_base) {
            bool eq = true;
            for (int w = 0; w < wA; w++) if (c.intent[w] != Y[w]) { eq = false; break; }
            if (eq) { dup = true; break; }
        }
        if (!dup) {
            Concept c(wB, wA);
            c.intent = Y;
            arrow_down(c.intent, R_fast_T_flat, wA, wB, B, c.extent);
            c.coverage = 0;
            c.extent_size = count_bits(c.extent);
            c.intent_size = count_bits(c.intent);
            c.base_id = (int)C1_base.size();
            C1_base.push_back(std::move(c));
        }
    }

    int N_base = C1_base.size();
    std::vector<double> dist_cache(N_base * N_base, -1.0);
    int max_concepts = N_base * 2 + 1;
    
    std::vector<Concept> pool(max_concepts, Concept(wB, wA));
    std::vector<double> dm(max_concepts * max_concepts, 0.0);
    std::vector<double> min_rho(max_concepts);
    std::vector<int> best_j_arr(max_concepts);
    std::vector<bool> active(max_concepts);
    std::vector<bool> needs_coverage_update(N_base, true);

    std::vector<Concept> F;
    std::vector<Concept*> C2_iter; C2_iter.reserve(N_base * 4);
    std::vector<Concept*> C1; C1.reserve(max_concepts);

    while (total_ones > 0) {
        C2_iter.clear(); C1.clear();
        for (int i = 0; i < N_base; i++) {
            if (needs_coverage_update[i]) {
                C1_base[i].coverage = compute_coverage(C1_base[i].extent, C1_base[i].intent, R_un_T_flat, wA, wB);
                needs_coverage_update[i] = false;
            }
            if (C1_base[i].coverage == 0) continue;
            
            C1.push_back(&C1_base[i]); 
            C2_iter.push_back(&C1_base[i]);
        }
        if (C1.empty()) break;

        if (C1.size() > 1) {
            int n = C1.size();
            std::fill(active.begin(), active.begin() + max_concepts, false);
            for (int i = 0; i < n; i++) active[i] = true;
            int active_count = n;

            for (int i = 0; i < n; i++) { min_rho[i] = 2e10; best_j_arr[i] = -1; }
            Bitset64 x_inter_buf(wB);
            for (int i = 0; i < n - 1; i++) {
                for (int j = i + 1; j < n; j++) {
                    double d; int bi = C1[i]->base_id, bj = C1[j]->base_id;
                    if (bi >= 0 && bj >= 0 && dist_cache[bi * N_base + bj] >= 0) { d = dist_cache[bi * N_base + bj]; }
                    else { 
                        d = compute_rho(C1[i], C1[j], R_un_T_flat, wA, wB, x_inter_buf); 
                        if (bi >= 0 && bj >= 0) { dist_cache[bi * N_base + bj] = d; dist_cache[bj * N_base + bi] = d; } 
                    }
                    dm[i * max_concepts + j] = d; dm[j * max_concepts + i] = d;
                    if (d < min_rho[i]) { min_rho[i] = d; best_j_arr[i] = j; }
                    if (d < min_rho[j]) { min_rho[j] = d; best_j_arr[j] = i; }
                }
            }

            int cur_size = n;
            while (active_count > 1) {
                double global_min = 2e10; int gi = -1, gj = -1;
                for (int i = 0; i < cur_size; i++) {
                    if (!active[i]) continue;
                    if (best_j_arr[i] < 0 || !active[best_j_arr[i]]) {
                        min_rho[i] = 2e10; best_j_arr[i] = -1;
                        for (int j = 0; j < cur_size; j++) {
                            if (i != j && active[j] && dm[i * max_concepts + j] < min_rho[i]) {
                                min_rho[i] = dm[i * max_concepts + j]; best_j_arr[i] = j;
                            }
                        }
                    }
                    if (min_rho[i] < global_min) { global_min = min_rho[i]; gi = i; gj = best_j_arr[i]; }
                }
                if (gi < 0 || gj < 0) break;
                active[gi] = false; active[gj] = false; active_count -= 2;

                Concept* cnew = &pool[cur_size];
                for (int w = 0; w < wA; w++) cnew->intent[w] = C1[gi]->intent[w] & C1[gj]->intent[w];
                arrow_down(cnew->intent, R_fast_T_flat, wA, wB, B, cnew->extent);

                cnew->extent_size = count_bits(cnew->extent);
                cnew->intent_size = count_bits(cnew->intent);
                cnew->coverage = -1;
                cnew->base_id = -1;

                int ni = cur_size;
                C1.push_back(cnew); C2_iter.push_back(cnew);
                active[ni] = true; active_count++;
                min_rho[ni] = 2e10; best_j_arr[ni] = -1;
                cur_size++;

                for (int i = 0; i < ni; i++) {
                    if (!active[i]) continue;
                    double rho = compute_rho(C1[i], C1[ni], R_un_T_flat, wA, wB, x_inter_buf);
                    dm[i * max_concepts + ni] = rho; dm[ni * max_concepts + i] = rho;
                    if (rho < min_rho[i]) { min_rho[i] = rho; best_j_arr[i] = ni; }
                    if (rho < min_rho[ni]) { min_rho[ni] = rho; best_j_arr[ni] = i; }
                }
            }
        }

        int best_cov = 0, best_idx = -1;
        for (int i = 0; i < (int)C2_iter.size(); i++) {
            int max_area = C2_iter[i]->extent_size * C2_iter[i]->intent_size;
            if (max_area <= best_cov) continue;
            
            if (C2_iter[i]->coverage < 0) {
                C2_iter[i]->coverage = compute_coverage(C2_iter[i]->extent, C2_iter[i]->intent, R_un_T_flat, wA, wB);
            }
            if (C2_iter[i]->coverage > best_cov) { 
                best_cov = C2_iter[i]->coverage; 
                best_idx = i; 
            }
        }
        if (best_cov <= 0 || best_idx < 0) break;

        const Concept* Fb = C2_iter[best_idx];
        F.push_back(*Fb);
        for (int wa = 0; wa < wA; wa++) {
            uint64_t y = Fb->intent[wa];
            while (y) {
                int a = wa * 64 + __builtin_ctzll(y);
                uint64_t* row_ptr = &R_un_T_flat[a * wB];
                for (int wb = 0; wb < wB; wb++) {
                    uint64_t removed = row_ptr[wb] & Fb->extent[wb];
                    if (removed) { total_ones -= __builtin_popcountll(removed); row_ptr[wb] &= ~Fb->extent[wb]; }
                }
                y &= y - 1;
            }
        }

        std::vector<int> overlap;
        for (int i = 0; i < N_base; i++) {
            bool ox = false;
            for (int w = 0; w < wB && !ox; w++) if (C1_base[i].extent[w] & Fb->extent[w]) ox = true;
            if (!ox) continue;
            bool oy = false;
            for (int w = 0; w < wA && !oy; w++) if (C1_base[i].intent[w] & Fb->intent[w]) oy = true;
            if (oy) { overlap.push_back(i); needs_coverage_update[i] = true; }
        }
        for (int i : overlap) for (int j : overlap) dist_cache[i * N_base + j] = -1.0;
    }

    int K = F.size(); 
    // Invert U and V assignment for the original matrix orientation
    // U was extent of transposed (so it's intent of original), V was intent of transposed (so it's extent of original)
    // Actually: R^T = U_t * V_t  => R = V_t^T * U_t^T
    // So U_final = V_t^T and V_final = U_t^T
    Rcpp::LogicalMatrix U_final(B_orig, K), V_final(K, A_orig);
    for (int k = 0; k < K; k++) {
        // F[k].extent has size B (A_orig), F[k].intent has size A (B_orig)
        for (int i = 0; i < B_orig; i++) if (get_bit(F[k].intent, i)) U_final(i, k) = true;
        for (int j = 0; j < A_orig; j++) if (get_bit(F[k].extent, j)) V_final(k, j) = true;
    }

    auto _end_time = std::chrono::high_resolution_clock::now();
    double time_s = std::chrono::duration<double>(_end_time - _start_time).count();
    double mem_mb = 0.0;

    return Rcpp::List::create(Rcpp::Named("U") = U_final, Rcpp::Named("V") = V_final, Rcpp::Named("time_s") = time_s, Rcpp::Named("mem_mb") = mem_mb);
}
