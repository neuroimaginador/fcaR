#include <Rcpp.h>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <queue>

using namespace Rcpp;

typedef std::vector<uint64_t> Bitset64;

inline void set_bit(Bitset64& bs, int pos) { bs[pos >> 6] |= (1ULL << (pos & 63)); }
inline bool get_bit(const Bitset64& bs, int pos) { return (bs[pos >> 6] >> (pos & 63)) & 1ULL; }

struct HyperRect {
    int id;
    std::vector<int> T;
    std::vector<int> I;
    Bitset64 T_mask;
    Bitset64 I_mask;
    std::vector<int> active_wB;
    std::vector<int> active_wA;
    bool active;
    int version;

    HyperRect(int _id, int wB, int wA) : id(_id), active(true), version(0) {
        T_mask.assign(wB, 0ULL);
        I_mask.assign(wA, 0ULL);
    }
};

struct MergeCandidate {
    int id1, id2;
    int v1, v2;
    double ratio;
    int savings;

    bool operator<(const MergeCandidate& other) const {
        return ratio < other.ratio; // Max-heap
    }
};

// Extremely fast local FP computation using stack-allocated bitmasks
long long compute_local_new_fp_fast(const uint64_t* T_union, const uint64_t* I_union, 
                                    const std::vector<Bitset64>& DB, 
                                    const std::vector<Bitset64>& Covered, 
                                    int wA, int wB) {
    long long fp = 0;
    for (int wb = 0; wb < wB; wb++) {
        uint64_t mask = T_union[wb];
        while (mask) {
            int t = wb * 64 + __builtin_ctzll(mask);
            for (int w = 0; w < wA; w++) {
                uint64_t fp_bits = I_union[w] & (~DB[t][w]) & (~Covered[t][w]);
                if (fp_bits) fp += __builtin_popcountll(fp_bits);
            }
            mask &= (mask - 1);
        }
    }
    return fp;
}

// [[Rcpp::export]]
List hyper_plus_optimized_cpp(LogicalMatrix I_mat, List hyper_res, double beta = 0.1) {
    int num_tx = I_mat.nrow(), num_items = I_mat.ncol();
    int wA = (num_items + 63) / 64, wB = (num_tx + 63) / 64;

    std::vector<Bitset64> DB(num_tx, Bitset64(wA, 0ULL));
    long long total_ones = 0;
    for (int t = 0; t < num_tx; t++) {
        for (int j = 0; j < num_items; j++) if (I_mat(t, j)) { set_bit(DB[t], j); total_ones++; }
    }
    if (total_ones == 0) return hyper_res;

    LogicalMatrix res_U = hyper_res["U"], res_V = hyper_res["V"];
    int k_in = res_U.ncol();
    
    std::vector<HyperRect*> active_scdb;
    std::vector<HyperRect*> rect_by_id; // O(1) LOOKUP
    rect_by_id.reserve(k_in + 50000); 

    std::vector<std::vector<int>> item_to_rects(num_items);
    std::vector<std::vector<int>> tx_to_rects(num_tx);
    
    for (int f = 0; f < k_in; f++) {
        HyperRect* hr = new HyperRect(f, wB, wA);
        for (int t = 0; t < num_tx; t++) if (res_U(t, f)) { hr->T.push_back(t); set_bit(hr->T_mask, t); tx_to_rects[t].push_back(f); }
        for (int j = 0; j < num_items; j++) if (res_V(f, j)) { hr->I.push_back(j); set_bit(hr->I_mask, j); item_to_rects[j].push_back(f); }
        
        // Cache active words
        for (int w = 0; w < wB; w++) if (hr->T_mask[w]) hr->active_wB.push_back(w);
        for (int w = 0; w < wA; w++) if (hr->I_mask[w]) hr->active_wA.push_back(w);

        rect_by_id.push_back(hr);
        active_scdb.push_back(hr);
    }

    std::vector<Bitset64> Covered(num_tx, Bitset64(wA, 0ULL));
    for (auto* hr : active_scdb) {
        for (int t : hr->T) for (int w : hr->active_wA) Covered[t][w] |= hr->I_mask[w];
    }
    long long current_fp = 0;
    for (int t = 0; t < num_tx; t++) for (int w = 0; w < wA; w++) current_fp += __builtin_popcountll(Covered[t][w] & ~DB[t][w]);

    std::priority_queue<MergeCandidate> pq;
    std::vector<bool> seen(k_in + 50000, false);

    auto add_candidates = [&](HyperRect* m) {
        if (m->id >= (int)seen.size()) seen.resize(m->id + 10000, false);
        std::fill(seen.begin(), seen.end(), false);
        seen[m->id] = true;
        
        auto process_neighbor = [&](int nid) {
            if (seen[nid]) return;
            seen[nid] = true;
            
            if (nid >= (int)rect_by_id.size()) return;
            HyperRect* other = rect_by_id[nid];
            if (!other || !other->active) return;

            int st = 0; for(int w : m->active_wB) st += __builtin_popcountll(m->T_mask[w] & other->T_mask[w]);
            int si = 0; for(int w : m->active_wA) si += __builtin_popcountll(m->I_mask[w] & other->I_mask[w]);
            int sav = st + si;
            if(sav > 0) {
                // Use fast stack arrays (no heap allocation, no recomputation)
                uint64_t Tu_arr[wB], Iu_arr[wA];
                for(int w=0; w<wB; w++) Tu_arr[w] = m->T_mask[w] | other->T_mask[w];
                for(int w=0; w<wA; w++) Iu_arr[w] = m->I_mask[w] | other->I_mask[w];
                
                long long nfp = compute_local_new_fp_fast(Tu_arr, Iu_arr, DB, Covered, wA, wB);
                if ((double)(current_fp + nfp) / total_ones <= beta) {
                    double r = (nfp == 0) ? (double)sav * 1e12 : (double)sav / (double)nfp;
                    pq.push({m->id, other->id, m->version, other->version, r, sav});
                }
            }
        };

        for (int item : m->I) for (int nid : item_to_rects[item]) process_neighbor(nid);
        for (int tx : m->T) for (int nid : tx_to_rects[tx]) process_neighbor(nid);
    };

    for(auto* h : active_scdb) add_candidates(h);

    int next_id = k_in;
    while (!pq.empty()) {
        MergeCandidate top = pq.top(); pq.pop();
        
        HyperRect *h1 = rect_by_id[top.id1];
        HyperRect *h2 = rect_by_id[top.id2];
        
        if (!h1 || !h2 || !h1->active || !h2->active) continue;
        if (h1->version != top.v1 || h2->version != top.v2) continue;

        uint64_t Tu_arr[wB], Iu_arr[wA];
        for(int w=0; w<wB; w++) Tu_arr[w] = h1->T_mask[w] | h2->T_mask[w];
        for(int w=0; w<wA; w++) Iu_arr[w] = h1->I_mask[w] | h2->I_mask[w];
        
        long long real_nfp = compute_local_new_fp_fast(Tu_arr, Iu_arr, DB, Covered, wA, wB);
        if ((double)(current_fp + real_nfp) / total_ones > beta) continue;
        double real_ratio = (real_nfp == 0) ? (double)top.savings * 1e12 : (double)top.savings / (double)real_nfp;
        
        if (!pq.empty() && real_ratio < pq.top().ratio) {
            pq.push({top.id1, top.id2, h1->version, h2->version, real_ratio, top.savings});
            continue;
        }

        h1->active = false; h2->active = false;
        HyperRect* m = new HyperRect(next_id++, wB, wA);
        
        for (int wb = 0; wb < wB; wb++) {
            m->T_mask[wb] = h1->T_mask[wb] | h2->T_mask[wb];
            if (m->T_mask[wb]) {
                m->active_wB.push_back(wb);
                uint64_t mask = m->T_mask[wb];
                while (mask) {
                    int t = wb * 64 + __builtin_ctzll(mask);
                    m->T.push_back(t);
                    tx_to_rects[t].push_back(m->id);
                    mask &= (mask - 1);
                }
            }
        }
        for (int wa = 0; wa < wA; wa++) {
            m->I_mask[wa] = h1->I_mask[wa] | h2->I_mask[wa];
            if (m->I_mask[wa]) {
                m->active_wA.push_back(wa);
                uint64_t mask = m->I_mask[wa];
                while (mask) {
                    int j = wa * 64 + __builtin_ctzll(mask);
                    m->I.push_back(j);
                    item_to_rects[j].push_back(m->id);
                    mask &= (mask - 1);
                }
            }
        }
        
        for(int t : m->T) for(int w : m->active_wA) Covered[t][w] |= m->I_mask[w];
        current_fp += real_nfp;

        if (next_id > (int)rect_by_id.size()) rect_by_id.resize(next_id + 50000, nullptr);
        rect_by_id[m->id] = m;
        
        active_scdb.erase(std::remove(active_scdb.begin(), active_scdb.end(), h1), active_scdb.end());
        active_scdb.erase(std::remove(active_scdb.begin(), active_scdb.end(), h2), active_scdb.end());
        
        add_candidates(m);
        active_scdb.push_back(m);
        Rcpp::checkUserInterrupt();
    }

    int k_out = (int)active_scdb.size();
    LogicalMatrix U_mat(num_tx, k_out), V_mat(k_out, num_items);
    for (int f = 0; f < k_out; f++) {
        for (int t : active_scdb[f]->T) U_mat(t, f) = true;
        for (int j : active_scdb[f]->I) V_mat(f, j) = true;
    }
    
    // Free allocated memory
    for (auto* ptr : rect_by_id) {
        delete ptr;
    }
    
    return List::create(Named("U") = U_mat, Named("V") = V_mat, Named("n_factors") = k_out);
}
