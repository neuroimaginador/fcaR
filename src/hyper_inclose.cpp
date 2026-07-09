// =============================================================================
// HYPER-INCLOSE Algorithm
//
// Combines Hyper (Xiang et al. 2011) with In-Close (FCA) to generate 
// candidate hyperrectangles from formal intents instead of frequent itemsets.
// =============================================================================

#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <queue>

using namespace Rcpp;

// =============================================================================
// BITSET UTILS
// =============================================================================
using Bitset64 = std::vector<uint64_t>;

inline void set_bit(Bitset64& bs, int index) {
    bs[index / 64] |= (1ULL << (index % 64));
}

inline bool get_bit(const Bitset64& bs, int index) {
    return (bs[index / 64] & (1ULL << (index % 64))) != 0;
}

inline int popcount_bitset(const Bitset64& bs) {
    int sum = 0;
    for (uint64_t val : bs) sum += __builtin_popcountll(val);
    return sum;
}

// =============================================================================
// IN-CLOSE (Frequent Intent Miner)
// =============================================================================

struct Candidate {
    std::vector<int> I_vec;
    Bitset64 I_mask;
    std::vector<int> best_T;
    double price;

    bool operator>(const Candidate& other) const {
        return price > other.price;
    }
};

class InCloseFrequentSolver {
public:
    int n_objects;
    int n_attributes;
    int min_support;
    size_t N_BLOCKS_M; // Blocks for attributes
    size_t N_BLOCKS_N; // Blocks for objects

    std::vector<uint64_t> attr_cols;
    std::vector<uint64_t> obj_rows;
    std::vector<Candidate> candidates;

    InCloseFrequentSolver(int n_obj, int n_att, int min_sup) :
        n_objects(n_obj), n_attributes(n_att), min_support(min_sup) {
        N_BLOCKS_M = (n_attributes + 63) / 64;
        N_BLOCKS_N = (n_objects + 63) / 64;
        attr_cols.assign(n_attributes * N_BLOCKS_N, 0ULL);
        obj_rows.assign(N_BLOCKS_M * n_objects, 0ULL);
    }

    void solve_recursive(int y, const std::vector<int>& extent, const Bitset64& intent) {
        // Current concept intent is a candidate
        Candidate cand;
        cand.I_mask = intent;
        for (int j = 0; j < n_attributes; j++) {
            if (get_bit(intent, j)) cand.I_vec.push_back(j);
        }
        if (!cand.I_vec.empty()) {
            candidates.push_back(cand);
        }

        std::vector<int> child_extent;
        child_extent.reserve(extent.size());
        Bitset64 child_intent(N_BLOCKS_M, 0ULL);

        for (int j = y + 1; j < n_attributes; j++) {
            if (get_bit(intent, j)) continue;

            child_extent.clear();
            for (int obj_idx : extent) {
                if ((attr_cols[j * N_BLOCKS_N + (obj_idx >> 6)] & (1ULL << (obj_idx & 63))) != 0) {
                    child_extent.push_back(obj_idx);
                }
            }

            if (child_extent.size() < (size_t)min_support) continue;

            for (size_t k = 0; k < N_BLOCKS_M; k++) {
                uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
                for (int obj_idx : child_extent) {
                    intent_k &= obj_rows[k * n_objects + obj_idx];
                }
                child_intent[k] = intent_k;
            }

            bool is_canonical = true;
            for (int b = 0; b < (j >> 6); b++) {
                if ((child_intent[b] & (~intent[b])) != 0) { is_canonical = false; break; }
            }
            if (is_canonical) {
                uint64_t new_bits = child_intent[j >> 6] & (~intent[j >> 6]);
                uint64_t mask = (1ULL << (j & 63)) - 1;
                if ((new_bits & mask) != 0) is_canonical = false;
            }

            if (is_canonical) {
                solve_recursive(j, child_extent, child_intent);
            }
        }
    }
};

// =============================================================================
// Algorithm 2: FindHyper
// =============================================================================

struct FindHyperResult {
    std::vector<int> T;
    double price;
};

FindHyperResult find_hyper(
    const std::vector<int>& I_vec,
    const Bitset64& I_mask,
    const std::vector<Bitset64>& DB,
    const std::vector<Bitset64>& U,
    int num_tx,
    int wA)
{
    struct TxGain { int id; int gain; };
    std::vector<TxGain> candidates;

    for (int t = 0; t < num_tx; t++) {
        bool supports = true;
        for (int item : I_vec) {
            if (!get_bit(DB[t], item)) { supports = false; break; }
        }
        if (!supports) continue;

        int gain = 0;
        for (int w = 0; w < wA; w++) {
            gain += __builtin_popcountll(U[t][w] & I_mask[w]);
        }
        if (gain > 0) {
            candidates.push_back({t, gain});
        }
    }

    if (candidates.empty()) return {{}, 1e18};

    std::sort(candidates.begin(), candidates.end(),
        [](const TxGain& a, const TxGain& b) { return a.gain > b.gain; });

    int I_size = (int)I_vec.size();
    std::vector<int> T;
    T.push_back(candidates[0].id);
    int total_gain = candidates[0].gain;
    double price = (double)(1 + I_size) / (double)total_gain;

    for (size_t idx = 1; idx < candidates.size(); idx++) {
        int new_gain = total_gain + candidates[idx].gain;
        int new_T_size = (int)T.size() + 1;
        double new_price = (double)(new_T_size + I_size) / (double)new_gain;

        if (new_price <= price) {
            T.push_back(candidates[idx].id);
            total_gain = new_gain;
            price = new_price;
        } else {
            break;
        }
    }

    return {T, price};
}

// =============================================================================
// Main Export: hyper_inclose_cpp
// =============================================================================

// [[Rcpp::export]]
List hyper_inclose_cpp(LogicalMatrix I_mat, int min_support = 1) {
    int num_tx = I_mat.nrow();
    int num_items = I_mat.ncol();
    int wA = (num_items + 63) / 64;

    // 1. Build DB as bitsets
    std::vector<Bitset64> DB(num_tx, Bitset64(wA, 0ULL));
    long long total_uncovered = 0;
    for (int t = 0; t < num_tx; t++) {
        for (int j = 0; j < num_items; j++) {
            if (I_mat(t, j)) {
                set_bit(DB[t], j);
                total_uncovered++;
            }
        }
    }

    if (total_uncovered == 0) {
        return List::create(Named("U") = LogicalMatrix(num_tx, 0),
                            Named("V") = LogicalMatrix(num_items, 0),
                            Named("n_factors") = 0);
    }

    // 2. Generate Candidate Intents using In-Close
    InCloseFrequentSolver solver(num_tx, num_items, min_support);
    for (int j = 0; j < num_items; j++) {
        for (int i = 0; i < num_tx; i++) {
            if (I_mat(i, j)) {
                solver.attr_cols[j * solver.N_BLOCKS_N + (i >> 6)] |= (1ULL << (i & 63));
                solver.obj_rows[(j >> 6) * num_tx + i] |= (1ULL << (j & 63));
            }
        }
    }

    // --- ADD SINGLETONS (Paper C_α = F_α ∪ I) ---
    // This ensures coverage and allows "cheap" small factors.
    for (int j = 0; j < num_items; j++) {
        Candidate cand;
        cand.I_vec = {j};
        cand.I_mask = Bitset64(wA, 0ULL);
        set_bit(cand.I_mask, j);
        solver.candidates.push_back(cand);
    }

    std::vector<int> initial_extent(num_tx);
    for(int i = 0; i < num_tx; i++) initial_extent[i] = i;

    Bitset64 initial_intent(wA, 0ULL);
    for (int k = 0; k < wA; k++) {
        uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
        for (int i = 0; i < num_tx; i++) intent_k &= solver.obj_rows[k * num_tx + i];
        initial_intent[k] = intent_k;
    }

    solver.solve_recursive(-1, initial_extent, initial_intent);

    // 3. Initial FindHyper and Priority Queue
    std::vector<Bitset64> U = DB;
    std::priority_queue<Candidate, std::vector<Candidate>, std::greater<Candidate>> pq;

    for (auto& cand : solver.candidates) {
        auto res = find_hyper(cand.I_vec, cand.I_mask, DB, U, num_tx, wA);
        cand.best_T = res.T;
        cand.price = res.price;
        if (cand.price < 1e17) pq.push(cand);
    }

    // 4. Main Hyper Loop
    std::vector<std::vector<int>> out_T, out_I;
    while (total_uncovered > 0 && !pq.empty()) {
        Candidate top = pq.top();
        pq.pop();

        auto res = find_hyper(top.I_vec, top.I_mask, DB, U, num_tx, wA);
        top.best_T = res.T;
        top.price = res.price;

        if (top.price >= 1e17) continue;

        if (!pq.empty() && top.price > pq.top().price) {
            pq.push(top);
            continue;
        }

        out_T.push_back(top.best_T);
        out_I.push_back(top.I_vec);

        for (int t : top.best_T) {
            for (int w = 0; w < wA; w++) {
                uint64_t removed = U[t][w] & top.I_mask[w];
                total_uncovered -= __builtin_popcountll(removed);
                U[t][w] &= ~top.I_mask[w];
            }
        }

        auto updated = find_hyper(top.I_vec, top.I_mask, DB, U, num_tx, wA);
        top.best_T = updated.T;
        top.price = updated.price;
        if (top.price < 1e17) pq.push(top);

        Rcpp::checkUserInterrupt();
    }

    // 5. Package Output
    int k = (int)out_T.size();
    LogicalMatrix U_mat(num_tx, k);
    LogicalMatrix V_mat(k, num_items);
    for (int f = 0; f < k; f++) {
        for (int t : out_T[f]) U_mat(t, f) = true;
        for (int j : out_I[f]) V_mat(f, j) = true;
    }

    return List::create(
        Named("U") = U_mat,
        Named("V") = V_mat,
        Named("n_factors") = k
    );
}
