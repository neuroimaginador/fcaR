#include <Rcpp.h>
#include <chrono>
#include <vector>
#include <deque>
#include <string>
#include <map>
#include <algorithm>

using namespace Rcpp;

namespace {

// Fast bitset with Small-Object Optimization (SBO)
class FastBitset {
public:
  uint64_t inline_words[4];
  std::vector<uint64_t> heap_words;
  uint64_t* ptr;
  int n_words;
  int n_bits;

  FastBitset() : ptr(nullptr), n_words(0), n_bits(0) {}
  
  void init(int n) {
    n_bits = n;
    n_words = (n + 63) / 64;
    if (n_words <= 4) {
      ptr = inline_words;
      for (int i=0; i<4; ++i) inline_words[i] = 0;
    } else {
      heap_words.assign(n_words, 0);
      ptr = heap_words.data();
    }
  }
  
  FastBitset(const FastBitset& other) : n_words(other.n_words), n_bits(other.n_bits) {
    if (n_words <= 4) {
      ptr = inline_words;
      for (int i=0; i<n_words; ++i) inline_words[i] = other.ptr[i];
    } else {
      heap_words = other.heap_words;
      ptr = heap_words.data();
    }
  }

  FastBitset& operator=(const FastBitset& other) {
    if (this == &other) return *this;
    n_bits = other.n_bits;
    int old_words = n_words;
    n_words = other.n_words;
    if (n_words <= 4) {
      if (old_words > 4) std::vector<uint64_t>().swap(heap_words); 
      ptr = inline_words;
      for (int i=0; i<n_words; ++i) inline_words[i] = other.ptr[i];
    } else {
      heap_words = other.heap_words;
      ptr = heap_words.data();
    }
    return *this;
  }
  
  FastBitset(FastBitset&& other) noexcept : n_words(other.n_words), n_bits(other.n_bits) {
    if (n_words <= 4) {
      ptr = inline_words;
      for (int i=0; i<n_words; ++i) inline_words[i] = other.ptr[i];
    } else {
      heap_words = std::move(other.heap_words);
      ptr = heap_words.data();
    }
    other.ptr = nullptr;
    other.n_words = 0;
    other.n_bits = 0;
  }
  
  FastBitset& operator=(FastBitset&& other) noexcept {
    if (this == &other) return *this;
    n_bits = other.n_bits;
    n_words = other.n_words;
    if (n_words <= 4) {
      std::vector<uint64_t>().swap(heap_words); 
      ptr = inline_words;
      for (int i=0; i<n_words; ++i) inline_words[i] = other.ptr[i];
    } else {
      heap_words = std::move(other.heap_words);
      ptr = heap_words.data();
    }
    other.ptr = nullptr;
    other.n_words = 0;
    other.n_bits = 0;
    return *this;
  }

  inline void set(int i) {
    ptr[i >> 6] |= (1ULL << (i & 63));
  }
  inline void set() { 
    std::fill(ptr, ptr + n_words, ~0ULL);
    int rem = n_bits & 63;
    if (rem > 0) ptr[n_words - 1] &= (1ULL << rem) - 1;
  }
  inline void reset() {
    std::fill(ptr, ptr + n_words, 0ULL);
  }
  inline void reset(int i) {
    ptr[i >> 6] &= ~(1ULL << (i & 63));
  }
  inline bool test(int i) const {
    return (ptr[i >> 6] & (1ULL << (i & 63))) != 0;
  }
  inline bool operator[](int i) const { return test(i); }
  
  inline FastBitset operator&(const FastBitset& other) const {
    FastBitset res;
    res.init(n_bits);
    for (int i = 0; i < n_words; ++i) res.ptr[i] = ptr[i] & other.ptr[i];
    return res;
  }
  inline FastBitset operator|(const FastBitset& other) const {
    FastBitset res;
    res.init(n_bits);
    for (int i = 0; i < n_words; ++i) res.ptr[i] = ptr[i] | other.ptr[i];
    return res;
  }
  inline FastBitset operator~() const {
    FastBitset res;
    res.init(n_bits);
    for (int i = 0; i < n_words; ++i) res.ptr[i] = ~ptr[i];
    int rem = n_bits & 63;
    if (rem > 0) {
      res.ptr[n_words - 1] &= (1ULL << rem) - 1;
    }
    return res;
  }

  inline FastBitset& operator|=(const FastBitset& other) {
    for (int i = 0; i < n_words; ++i) ptr[i] |= other.ptr[i];
    return *this;
  }
  inline FastBitset& operator&=(const FastBitset& other) {
    for (int i = 0; i < n_words; ++i) ptr[i] &= other.ptr[i];
    return *this;
  }

  inline bool any() const {
    for (int i = 0; i < n_words; ++i) {
      if (ptr[i] != 0) return true;
    }
    return false;
  }
  inline bool none() const {
    for (int i = 0; i < n_words; ++i) {
      if (ptr[i] != 0) return false;
    }
    return true;
  }
  inline int count() const {
    int c = 0;
    for (int i = 0; i < n_words; ++i) c += __builtin_popcountll(ptr[i]);
    return c;
  }

  // To match boost npos
  static const size_t npos = -1;

  inline size_t find_first() const {
    for (int i = 0; i < n_words; ++i) {
      if (ptr[i] != 0) return (i << 6) + __builtin_ctzll(ptr[i]);
    }
    return static_cast<size_t>(-1);
  }
  inline size_t find_next(size_t prev) const {
    size_t next_idx = prev + 1;
    if (next_idx >= (size_t)n_bits) return static_cast<size_t>(-1);
    int word_idx = next_idx >> 6;
    uint64_t mask = ~0ULL << (next_idx & 63);
    uint64_t masked_word = ptr[word_idx] & mask;
    if (masked_word != 0) return (word_idx << 6) + __builtin_ctzll(masked_word);
    for (int i = word_idx + 1; i < n_words; ++i) {
      if (ptr[i] != 0) return (i << 6) + __builtin_ctzll(ptr[i]);
    }
    return static_cast<size_t>(-1);
  }

  inline bool operator==(const FastBitset& other) const {
    for (int i = 0; i < n_words; ++i) if (ptr[i] != other.ptr[i]) return false;
    return true;
  }
  inline bool operator!=(const FastBitset& other) const {
    return !(*this == other);
  }
  
  inline bool operator<(const FastBitset& other) const {
    for (int i = n_words - 1; i >= 0; --i) {
      if (ptr[i] < other.ptr[i]) return true;
      if (ptr[i] > other.ptr[i]) return false;
    }
    return false;
  }
  
  // Custom helper subset checks
  inline bool is_subset_of(const FastBitset& other) const {
      for (int i = 0; i < n_words; ++i) {
          if ((ptr[i] & ~other.ptr[i]) != 0) return false;
      }
      return true;
  }
};


using Bitset = FastBitset;

// Estructura basica para la base
struct SigmaRule {
    Bitset lhs;
    Bitset rhs;
    bool is_redundant;
};

// =============================================================================
// --- METRICS ---
// =============================================================================
struct BinaryMetrics {
    double execution_time = 0;
    int iterations = 0;
    int iterations_saturate = 0;
    int iterations_prune = 0;
    int pi_calculations = 0;
    int add_derived_calls = 0;
    int final_implication_count = 0;
    int worklist_max_size = 0;
};

List Metrics_to_R(const BinaryMetrics& m) {
    return List::create(
        Named("execution_time") = m.execution_time,
        Named("iterations") = m.iterations,
        Named("iterations_saturate") = m.iterations_saturate,
        Named("iterations_prune") = m.iterations_prune,
        Named("pi_calculations") = m.pi_calculations,
        Named("add_derived_calls") = m.add_derived_calls,
        Named("final_implication_count") = m.final_implication_count,
        Named("worklist_max_size") = m.worklist_max_size
    );
}

// =============================================================================
// --- CONVERSION DE ENTRADA / SALIDA ---
// =============================================================================

// Sparse Matrix a un Vector de Reglas
std::vector<SigmaRule> Sparse_to_BinarySigma(S4 lhs_mat, S4 rhs_mat, int& n_attributes) {
    std::vector<SigmaRule> sigma;
    IntegerVector i_lhs = lhs_mat.slot("i");
    IntegerVector p_lhs = lhs_mat.slot("p");
    IntegerVector dim_lhs = lhs_mat.slot("Dim");
    int ncols = dim_lhs[1];
    n_attributes = dim_lhs[0];

    IntegerVector i_rhs = rhs_mat.slot("i");
    IntegerVector p_rhs = rhs_mat.slot("p");

    for (int col = 0; col < ncols; ++col) {
        Bitset current_lhs; current_lhs.init(n_attributes);
        int start_lhs = p_lhs[col];
        int end_lhs = p_lhs[col + 1];
        for (int k = start_lhs; k < end_lhs; ++k) {
            current_lhs.set(i_lhs[k]);
        }

        Bitset current_rhs; current_rhs.init(n_attributes);
        int start_rhs = p_rhs[col];
        int end_rhs = p_rhs[col + 1];
        for (int k = start_rhs; k < end_rhs; ++k) {
            current_rhs.set(i_rhs[k]);
        }

        if (current_rhs.count() > 0) {
            sigma.push_back({current_lhs, current_rhs, false});
        }
    }
    return sigma;
}

List BinarySigma_to_R(const std::vector<SigmaRule>& s, int n_attributes) {
    std::vector<int> lhs_i, lhs_j, rhs_i, rhs_j;
    std::vector<double> lhs_x, rhs_x; // mantengo X como double para compatibilidad con dgCMatrix en fcaR
    int col_idx = 0;

    for (const auto& rule : s) {
        if (rule.is_redundant) continue;

        for (size_t r = rule.lhs.find_first(); r != Bitset::npos; r = rule.lhs.find_next(r)) {
            lhs_i.push_back(r);
            lhs_j.push_back(col_idx);
            lhs_x.push_back(1.0);
        }
        for (size_t r = rule.rhs.find_first(); r != Bitset::npos; r = rule.rhs.find_next(r)) {
            rhs_i.push_back(r);
            rhs_j.push_back(col_idx);
            rhs_x.push_back(1.0);
        }
        col_idx++;
    }
    return List::create(
        Named("lhs_i") = lhs_i, Named("lhs_j") = lhs_j, Named("lhs_x") = lhs_x,
        Named("rhs_i") = rhs_i, Named("rhs_j") = rhs_j, Named("rhs_x") = rhs_x,
        Named("n_rules") = col_idx
    );
}

// =============================================================================
// --- CORE ALGORITHMIC OPERATIONS (BINARY FAST) ---
// =============================================================================

struct TrieNode {
    int attr;
    int first_child;
    int next_sibling;
    Bitset rhs;
    bool has_rule;
};

class BinaryTrie {
public:
    std::vector<TrieNode> nodes;
    int n_bits;
    
    BinaryTrie(int bits) : n_bits(bits) {
        nodes.push_back({-1, -1, -1, Bitset(), false});
        nodes[0].rhs.init(n_bits);
    }
    
    void clear() {
        nodes.clear();
        nodes.push_back({-1, -1, -1, Bitset(), false});
        nodes[0].rhs.init(n_bits);
    }

    int add_rule(const Bitset& lhs, const Bitset& rhs) {
        int curr = 0;
        for (size_t i = lhs.find_first(); i != Bitset::npos; i = lhs.find_next(i)) {
            int child = nodes[curr].first_child;
            int prev_sibling = -1;
            bool found = false;
            
            while (child != -1) {
                if (nodes[child].attr == (int)i) {
                    curr = child;
                    found = true;
                    break;
                } else if (nodes[child].attr > (int)i) {
                    break;
                }
                prev_sibling = child;
                child = nodes[child].next_sibling;
            }
            
            if (!found) {
                int new_node_idx = nodes.size();
                nodes.push_back({(int)i, -1, child, Bitset(), false});
                nodes.back().rhs.init(n_bits);
                
                if (prev_sibling == -1) {
                    nodes[curr].first_child = new_node_idx;
                } else {
                    nodes[prev_sibling].next_sibling = new_node_idx;
                }
                curr = new_node_idx;
            }
        }
        
        nodes[curr].has_rule = true;
        nodes[curr].rhs = rhs;
        return curr;
    }

    void calculate_closure_recursive(int node_idx, const Bitset& A, Bitset& closure) const {
        if (nodes[node_idx].has_rule) {
            closure |= nodes[node_idx].rhs;
        }
        
        int child = nodes[node_idx].first_child;
        while (child != -1) {
            if (A.test(nodes[child].attr)) {
                calculate_closure_recursive(child, A, closure);
            }
            child = nodes[child].next_sibling;
        }
    }
};

Bitset calculate_pi_operator_tree(const Bitset& A, const BinaryTrie& trie, BinaryMetrics& metrics) {
    metrics.pi_calculations++;
    Bitset closure = A;
    trie.calculate_closure_recursive(0, A, closure);
    return closure;
}


// Operador Pi (una pasada)
Bitset calculate_pi_operator_binary(Bitset A, const std::vector<SigmaRule>& sigma, BinaryMetrics& metrics) {
    metrics.pi_calculations++;
    Bitset closure = A;
    // Fast binary check
    for(const auto& rule : sigma) {
        if (!rule.is_redundant && rule.lhs.is_subset_of(A)) {
            closure |= rule.rhs;
        }
    }
    return closure;
}

void add_derived_binary(
    const Bitset& A, const Bitset& B, const Bitset& C, const Bitset& D,
    std::vector<Bitset>& derived_lhs, std::vector<Bitset>& derived_rhs,
    bool use_pruning, BinaryMetrics& metrics, int n_attributes
) {
    metrics.add_derived_calls++;
    
    // G = A U (C \ B)
    Bitset G = A | (C & ~B);
    
    // H = D \ A
    Bitset H = D & ~A;
    if (H.none()) return; // Vacio

    Bitset H_sem_reduced = H;
    if (use_pruning) {
        // En Pi, la base local es {(A->B), (C->D)}
        std::vector<SigmaRule> local_sigma = {
            {A, B, false},
            {C, D, false}
        };
        Bitset local_closure = calculate_pi_operator_binary(G, local_sigma, metrics);
        // H \ pi(G)
        H_sem_reduced &= ~local_closure;
    }

    // H_final = H_sem_reduced \ G
    Bitset H_final = H_sem_reduced & ~G;
    
    if (H_final.any()) {
        derived_lhs.push_back(G);
        derived_rhs.push_back(H_final);
    }
}

// Buscar una regla con este LHS en el sistema (O(N) search)
int find_rule(const std::vector<SigmaRule>& sigma, const Bitset& lhs) {
    for(size_t i = 0; i < sigma.size(); ++i) {
        if (sigma[i].lhs == lhs) return i;
    }
    return -1;
}

// =============================================================================
// --- NEW ALGORITHM 4: BATCH-BITWISE PI OPERATOR ---
// =============================================================================

struct BatchBitwiseSystem {
    int n_attributes;
    int n_rules;
    std::vector<Bitset> T_L; // size n_attributes, each Bitset has length n_rules
    std::vector<Bitset> T_R; // size n_attributes, each Bitset has length n_rules
    Bitset valid_rules;      // length n_rules, bit i is 1 if rule i is active (not redundant)
    BatchBitwiseSystem(int attrs) : n_attributes(attrs), n_rules(0) {
        T_L.resize(attrs);
        T_R.resize(attrs);
    }

    void add_rule(const Bitset& lhs, const Bitset& rhs, bool redundant = false) {
        // Here we're appending bit tests across.
        // FastBitset is dynamic up to its 'n_bits', but appending bit by bit requires resizing the capacity.
        // It's much simpler to use std::vector<bool> for columnar processing (since it's transposed) or resize FastBitset
        // Since T_L represents columns of bits, we manually rebuild length.
        // Let's use std::vector<bool> or a standard container here.
        // Actually, let's keep std::vector<FastBitset> but we need a .push_back(bit) logic.
        // To make it fully transparent and keep things quick without rebuilding string implementations:
        
        // As a trick: we know the maximum possible number of rules from W size. We can allocate beforehand. 
        // But for dynamic:
        for (int j = 0; j < n_attributes; ++j) {
            // we resize the columns:
            if (T_L[j].n_bits <= n_rules) {
                // Resize FastBitset
                FastBitset new_L; new_L.init(n_rules + 64);
                FastBitset new_R; new_R.init(n_rules + 64);
                for(int i=0; i<n_rules; ++i) {
                    if (T_L[j].test(i)) new_L.set(i);
                    if (T_R[j].test(i)) new_R.set(i);
                }
                T_L[j] = new_L;
                T_R[j] = new_R;
            }
            if (lhs.test(j)) T_L[j].set(n_rules);
            else T_L[j].reset(n_rules);
            if (rhs.test(j)) T_R[j].set(n_rules);
            else T_R[j].reset(n_rules);
        }
        
        if (valid_rules.n_bits <= n_rules) {
            FastBitset new_v; new_v.init(n_rules + 64);
            for(int i=0; i<n_rules; ++i) {
                if (valid_rules.test(i)) new_v.set(i);
            }
            valid_rules = new_v;
        }
        if (!redundant) valid_rules.set(n_rules);
        else valid_rules.reset(n_rules);
        n_rules++;
    }

    void set_redundant(int rule_idx, bool redundant) {
        if (!redundant) valid_rules.set(rule_idx);
        else valid_rules.reset(rule_idx);
    }
    
    // Updates an existing rule's RHS (for Monotonic merging)
    void update_rhs(int rule_idx, const Bitset& new_rhs) {
        for (int j = 0; j < n_attributes; ++j) {
            if (new_rhs.test(j)) {
                T_R[j].set(rule_idx);
            } else {
                T_R[j].reset(rule_idx);
            }
        }
    }

    Bitset calculate_pi(Bitset A, BinaryMetrics& metrics) const {
        metrics.pi_calculations++;
        
        Bitset unsatisfied; unsatisfied.init(n_rules); // All 0 initially
        
        // Iterate over attributes missing from A
        Bitset not_A = ~A;
        for (size_t j = not_A.find_first(); j != Bitset::npos; j = not_A.find_next(j)) {
            if (j < (size_t)n_attributes) {
                unsatisfied |= T_L[j];
            }
        }
        
        Bitset satisfied = ~unsatisfied;
        satisfied &= valid_rules; // Only consider active rules
        
        Bitset result = A;
        for (int j = 0; j < n_attributes; ++j) {
            if ((satisfied & T_R[j]).any()) {
                result.set(j);
            }
        }
        return result;
    }
};

std::vector<SigmaRule> internal_run_monotonic_batch_bitwise(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    std::vector<SigmaRule> sigma_do;
    BatchBitwiseSystem batch_sys(n_attributes);

    std::deque<SigmaRule> W;
    for(const auto& r : sigma_in) W.push_back(r);

    while(!W.empty()) {
        metrics.iterations++;
        if (W.size() > (size_t)metrics.worklist_max_size) metrics.worklist_max_size = W.size();
        
        SigmaRule imp = W.front(); W.pop_front();
        Bitset A = imp.lhs;
        Bitset B = imp.rhs;

        Bitset B_new = B & ~batch_sys.calculate_pi(A, metrics);
        bool propagate = false;
        Bitset B_for_propagation = B_new;

        int existing_idx = find_rule(sigma_do, A);
        if (existing_idx == -1) {
            if (B_new.any()) {
                sigma_do.push_back({A, B_new, false});
                batch_sys.add_rule(A, B_new, false);
                propagate = true;
            }
        } else {
            Bitset B_old = sigma_do[existing_idx].rhs;
            Bitset B_merged = B_old | B_new;
            B_for_propagation = B_merged;

            if (B_merged != B_old) {
                sigma_do[existing_idx].rhs = B_merged;
                batch_sys.update_rhs(existing_idx, B_merged);
                propagate = true;
            }
        }

        if (propagate) {
            int current_n_rules = sigma_do.size();
            for(int i = 0; i < current_n_rules; ++i) {
                if (sigma_do[i].is_redundant) continue;
                if (sigma_do[i].lhs == A) continue;

                Bitset C = sigma_do[i].lhs;
                Bitset D = sigma_do[i].rhs;

                if (A.is_subset_of(C)) {
                    Bitset diff = B_new & ~C;
                    if (diff.any()) {
                        W.push_back({C, D, false});
                    }
                }

                std::vector<Bitset> d_lhs, d_rhs;
                add_derived_binary(A, B_for_propagation, C, D, d_lhs, d_rhs, use_pruning, metrics, n_attributes);
                add_derived_binary(C, D, A, B_for_propagation, d_lhs, d_rhs, use_pruning, metrics, n_attributes);

                for(size_t k = 0; k < d_lhs.size(); ++k) {
                    W.push_back({d_lhs[k], d_rhs[k], false});
                }
            }
        }
    }

    // Pruning Final
    while(true) {
        metrics.iterations_prune++;
        bool made_changes = false;
        
        for (size_t i = 0; i < sigma_do.size(); ++i) {
            if (sigma_do[i].is_redundant) continue;

            Bitset A = sigma_do[i].lhs;
            Bitset rhs_val = sigma_do[i].rhs;
            
            sigma_do[i].is_redundant = true; 
            batch_sys.set_redundant(i, true);

            Bitset D = batch_sys.calculate_pi(A, metrics);
            Bitset B_new = rhs_val & ~D;

            if (B_new.any()) {
                sigma_do[i].is_redundant = false; 
                batch_sys.set_redundant(i, false);
                if (sigma_do[i].rhs != B_new) {
                    sigma_do[i].rhs = B_new;
                    batch_sys.update_rhs(i, B_new);
                    made_changes = true;
                }
            } else {
                made_changes = true; 
            }
        }
        if (!made_changes) break;
    }

    return sigma_do;
}

// =============================================================================
// --- ALGORITHM 1: MONOTONIC INCREMENTAL (BINARY) ---
// =============================================================================

std::vector<SigmaRule> internal_run_monotonic_binary(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    std::vector<SigmaRule> sigma_do; // Base actual
    std::deque<SigmaRule> W;
    for(const auto& r : sigma_in) W.push_back(r);

    while(!W.empty()) {
        metrics.iterations++;
        if (W.size() > (size_t)metrics.worklist_max_size) metrics.worklist_max_size = W.size();
        
        SigmaRule imp = W.front(); W.pop_front();
        Bitset A = imp.lhs;
        Bitset B = imp.rhs;

        Bitset B_new = B & ~calculate_pi_operator_binary(A, sigma_do, metrics);
        bool propagate = false;
        Bitset B_for_propagation = B_new;

        int existing_idx = find_rule(sigma_do, A);
        if (existing_idx == -1) {
            if (B_new.any()) {
                sigma_do.push_back({A, B_new, false});
                propagate = true;
            }
        } else {
            Bitset B_old;
            if (sigma_do[existing_idx].is_redundant) {
                B_old.init(A.n_bits);
            } else {
                B_old = sigma_do[existing_idx].rhs;
            }
            Bitset B_merged = B_old | B_new;
            B_for_propagation = B_merged;

            if (sigma_do[existing_idx].is_redundant || B_merged != B_old) {
                sigma_do[existing_idx].rhs = B_merged;
                sigma_do[existing_idx].is_redundant = false;
                propagate = true;
            }
        }

        if (propagate) {
            // Propagate over sigma_do
            int current_n_rules = sigma_do.size(); // To avoid iterating over newly added derived rules in the same pass
            for(int i = 0; i < current_n_rules; ++i) {
                if (sigma_do[i].is_redundant) continue;
                if (sigma_do[i].lhs == A) continue;

                Bitset C = sigma_do[i].lhs;
                Bitset D = sigma_do[i].rhs;

                // Propagacion hacia atras optimizada
                if (A.is_subset_of(C)) {
                    Bitset diff = B_new & ~C;
                    if (diff.any()) {
                        W.push_back({C, D, false});
                    }
                }

                // Generar nuevas reglas cruzadas
                std::vector<Bitset> d_lhs, d_rhs;
                add_derived_binary(A, B_for_propagation, C, D, d_lhs, d_rhs, use_pruning, metrics, n_attributes);
                add_derived_binary(C, D, A, B_for_propagation, d_lhs, d_rhs, use_pruning, metrics, n_attributes);

                for(size_t k = 0; k < d_lhs.size(); ++k) {
                    W.push_back({d_lhs[k], d_rhs[k], false});
                }
            }
        }
    }

    // Pruning Final
    while(true) {
        metrics.iterations_prune++;
        bool made_changes = false;
        
        for (size_t i = 0; i < sigma_do.size(); ++i) {
            if (sigma_do[i].is_redundant) continue;

            Bitset A = sigma_do[i].lhs;
            Bitset rhs_val = sigma_do[i].rhs;
            sigma_do[i].is_redundant = true; // Temporary remove

            Bitset D = calculate_pi_operator_binary(A, sigma_do, metrics);
            Bitset B_new = rhs_val & ~D;

            if (B_new.any()) {
                sigma_do[i].is_redundant = false; 
                if (sigma_do[i].rhs != B_new) {
                    sigma_do[i].rhs = B_new;
                    made_changes = true;
                }
            } else {
                made_changes = true; // It stayed redundant
            }
        }
        if (!made_changes) break;
    }

    return sigma_do;
}

// =============================================================================
// --- ALGORITHM 2 & 3: DO-SP and SINGLE-PASS (BINARY) ---
// =============================================================================

void saturate_system_binary(std::vector<SigmaRule>& sigma, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    bool made_changes = true;
    while(made_changes) {
        metrics.iterations_saturate++;
        made_changes = false;
        std::vector<Bitset> derived_lhs_pass, derived_rhs_pass;

        for (size_t i = 0; i < sigma.size(); ++i) {
            if(sigma[i].is_redundant) continue;
            for (size_t j = i; j < sigma.size(); ++j) {
                if(sigma[j].is_redundant) continue;

                add_derived_binary(sigma[i].lhs, sigma[i].rhs, sigma[j].lhs, sigma[j].rhs, derived_lhs_pass, derived_rhs_pass, use_pruning, metrics, n_attributes);
                if (i != j) {
                    add_derived_binary(sigma[j].lhs, sigma[j].rhs, sigma[i].lhs, sigma[i].rhs, derived_lhs_pass, derived_rhs_pass, use_pruning, metrics, n_attributes);
                }
            }
        }

        if (!derived_lhs_pass.empty()) {
            for (size_t k = 0; k < derived_lhs_pass.size(); ++k) {
                const Bitset& G = derived_lhs_pass[k];
                const Bitset& H = derived_rhs_pass[k];
                
                int existing_idx = find_rule(sigma, G);
                if (existing_idx == -1) {
                    made_changes = true;
                    sigma.push_back({G, H, false});
                } else {
                    Bitset B_old = sigma[existing_idx].rhs;
                    Bitset B_merged = B_old | H;
                    bool was_redundant = sigma[existing_idx].is_redundant;
                    
                    if (was_redundant || B_merged != B_old) {
                        made_changes = true;
                        sigma[existing_idx].rhs = B_merged;
                        sigma[existing_idx].is_redundant = false;
                    }
                }
            }
        }
    }
}

void saturate_system_single_pass_binary(std::vector<SigmaRule>& sigma, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    metrics.iterations_saturate++;
    std::vector<Bitset> derived_lhs_pass, derived_rhs_pass;

    for (size_t i = 0; i < sigma.size(); ++i) {
        if(sigma[i].is_redundant) continue;
        for (size_t j = 0; j < sigma.size(); ++j) {
            if(sigma[j].is_redundant) continue;
            add_derived_binary(sigma[i].lhs, sigma[i].rhs, sigma[j].lhs, sigma[j].rhs, derived_lhs_pass, derived_rhs_pass, use_pruning, metrics, n_attributes);
        }
    }

    if (!derived_lhs_pass.empty()) {
        for (size_t k = 0; k < derived_lhs_pass.size(); ++k) {
            const Bitset& G = derived_lhs_pass[k];
            Bitset H = derived_rhs_pass[k];
            
            Bitset closure_G = calculate_pi_operator_binary(G, sigma, metrics);
            H &= ~closure_G;
            if (H.none()) continue;
            
            int existing_idx = find_rule(sigma, G);
            if (existing_idx == -1) {
                sigma.push_back({G, H, false});
            } else {
                sigma[existing_idx].rhs |= H;
                sigma[existing_idx].is_redundant = false;
            }
        }
    }
}

void prune_system_binary(std::vector<SigmaRule>& sigma, BinaryMetrics& metrics) {
    while(true) {
        metrics.iterations_prune++;
        bool made_changes = false;
        
        for (size_t i = 0; i < sigma.size(); ++i) {
            if (sigma[i].is_redundant) continue;

            Bitset A = sigma[i].lhs;
            Bitset rhs_val = sigma[i].rhs;
            sigma[i].is_redundant = true; 

            Bitset D = calculate_pi_operator_binary(A, sigma, metrics);
            Bitset B_new = rhs_val & ~D;

            if (B_new.any()) {
                sigma[i].is_redundant = false; 
                if (sigma[i].rhs != B_new) {
                    sigma[i].rhs = B_new;
                    made_changes = true;
                }
            } else {
                made_changes = true; 
            }
        }
        if (!made_changes) break;
    }
}

std::vector<SigmaRule> internal_run_dosp_binary(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    saturate_system_binary(sigma_in, metrics, use_pruning, n_attributes);
    prune_system_binary(sigma_in, metrics);
    return sigma_in;
}

std::vector<SigmaRule> internal_run_single_pass_binary(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    while(true) {
        metrics.iterations++;
        std::vector<SigmaRule> sigma_old = sigma_in;
        
        saturate_system_single_pass_binary(sigma_in, metrics, use_pruning, n_attributes);
        prune_system_binary(sigma_in, metrics);

        bool same = true;
        if(sigma_in.size() == sigma_old.size()) {
            for(size_t i = 0; i < sigma_in.size(); ++i) {
                if(sigma_in[i].lhs != sigma_old[i].lhs || sigma_in[i].rhs != sigma_old[i].rhs || sigma_in[i].is_redundant != sigma_old[i].is_redundant) {
                    same = false; break;
                }
            }
        } else {
            same = false;
        }
        if(same) break;
    }
    return sigma_in;
}

// =============================================================================
// --- TREE ALGORITHMS ---
// =============================================================================

void saturate_system_tree_binary(std::vector<SigmaRule>& sigma, BinaryTrie& trie, std::vector<int>& trie_idx, BinaryMetrics& metrics, bool use_pruning, int n_attributes, const std::vector<bool>& is_modified) {
    metrics.iterations_saturate++;
    std::map<Bitset, Bitset> candidate_map;

    for (size_t i = 0; i < sigma.size(); ++i) {
        if(sigma[i].is_redundant) continue;
        for (size_t j = i + 1; j < sigma.size(); ++j) {
            if(sigma[j].is_redundant) continue;
            // Semi-Naive Delta Evaluation
            if (!is_modified[i] && !is_modified[j]) continue;

            // Forward A, B, C, D combination (G1, H1)
            Bitset G1 = sigma[i].lhs | (sigma[j].lhs & ~sigma[i].rhs);
            Bitset H1 = sigma[j].rhs & ~sigma[i].lhs;
            if (H1.any()) {
                Bitset H_final1 = H1 & ~G1;
                if (H_final1.any()) {
                    auto it = candidate_map.find(G1);
                    if (it == candidate_map.end()) candidate_map[G1] = H_final1;
                    else it->second |= H_final1;
                }
            }
            
            // Asymmetric Reverse C, D, A, B combination (G2, H2)
            Bitset G2 = sigma[j].lhs | (sigma[i].lhs & ~sigma[j].rhs);
            Bitset H2 = sigma[i].rhs & ~sigma[j].lhs;
            if (H2.any()) {
                Bitset H_final2 = H2 & ~G2;
                if (H_final2.any()) {
                    auto it = candidate_map.find(G2);
                    if (it == candidate_map.end()) candidate_map[G2] = H_final2;
                    else it->second |= H_final2;
                }
            }
        }
    }

    if (!candidate_map.empty()) {
        for (auto const& pair_item : candidate_map) {
            const Bitset& G = pair_item.first;
            const Bitset& H_accum = pair_item.second;
            Bitset H = H_accum;
            Bitset closure_G = calculate_pi_operator_tree(G, trie, metrics);
            H &= ~closure_G;
            if (H.none()) continue;
            
            int existing_idx = find_rule(sigma, G);
            if (existing_idx == -1) {
                sigma.push_back({G, H, false});
                trie_idx.push_back(trie.add_rule(G, H));
            } else {
                sigma[existing_idx].rhs |= H;
                sigma[existing_idx].is_redundant = false;
                trie.nodes[trie_idx[existing_idx]].rhs |= H;
                trie.nodes[trie_idx[existing_idx]].has_rule = true;
            }
        }
    }
}

void prune_system_tree_binary(std::vector<SigmaRule>& sigma, BinaryTrie& trie, std::vector<int>& trie_idx, BinaryMetrics& metrics) {
    while(true) {
        metrics.iterations_prune++;
        bool made_changes = false;
        
        for (size_t i = 0; i < sigma.size(); ++i) {
            if (sigma[i].is_redundant) continue;

            Bitset A = sigma[i].lhs;
            Bitset rhs_val = sigma[i].rhs;
            sigma[i].is_redundant = true; 
            trie.nodes[trie_idx[i]].has_rule = false; // Disable temporarily

            Bitset D = calculate_pi_operator_tree(A, trie, metrics);
            Bitset B_new = rhs_val & ~D;

            if (B_new.any()) {
                sigma[i].is_redundant = false; 
                trie.nodes[trie_idx[i]].has_rule = true;
                if (sigma[i].rhs != B_new) {
                    sigma[i].rhs = B_new;
                    trie.nodes[trie_idx[i]].rhs = B_new;
                    made_changes = true;
                }
            } else {
                made_changes = true; 
            }
        }
        if (!made_changes) break;
    }
}

std::vector<SigmaRule> internal_run_tree_binary(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    BinaryTrie trie(n_attributes);
    std::vector<int> trie_idx;
    for(size_t i=0; i<sigma_in.size(); ++i) {
        trie_idx.push_back(trie.add_rule(sigma_in[i].lhs, sigma_in[i].rhs));
    }

    std::vector<bool> is_modified(sigma_in.size(), true); // Initially all rules are "new"

    while(true) {
        metrics.iterations++;
        int old_size = sigma_in.size();
        std::vector<SigmaRule> sigma_old = sigma_in;
        
        saturate_system_tree_binary(sigma_in, trie, trie_idx, metrics, use_pruning, n_attributes, is_modified);
        
        is_modified.assign(sigma_in.size(), false);
        for(size_t i = old_size; i < sigma_in.size(); ++i) is_modified[i] = true;
        for(int i = 0; i < old_size; ++i) {
            if (sigma_in[i].rhs != sigma_old[i].rhs || sigma_in[i].is_redundant != sigma_old[i].is_redundant) {
                is_modified[i] = true;
            }
        }

        std::vector<SigmaRule> sigma_pre_prune = sigma_in;
        prune_system_tree_binary(sigma_in, trie, trie_idx, metrics);

        bool same = true;
        if(sigma_in.size() == sigma_old.size()) {
            for(size_t i = 0; i < sigma_in.size(); ++i) {
                if(sigma_in[i].lhs != sigma_old[i].lhs || sigma_in[i].rhs != sigma_old[i].rhs || sigma_in[i].is_redundant != sigma_old[i].is_redundant) {
                    same = false; break;
                }
            }
        } else {
            same = false;
        }
        
        for(size_t i = 0; i < sigma_in.size(); ++i) {
            if (sigma_in[i].rhs != sigma_pre_prune[i].rhs || sigma_in[i].is_redundant != sigma_pre_prune[i].is_redundant) {
                is_modified[i] = true;
            }
        }
        if(same) break;
    }
    return sigma_in;
}

// =============================================================================
// --- LEXICOGRAPHIC (MAP) ALGORITHMS ---
// =============================================================================

void saturate_system_lexicographic_binary(std::vector<SigmaRule>& sigma, std::map<Bitset, int>& lhs_map, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    metrics.iterations_saturate++;
    std::vector<Bitset> derived_lhs_pass, derived_rhs_pass;

    for (size_t i = 0; i < sigma.size(); ++i) {
        if(sigma[i].is_redundant) continue;
        for (size_t j = 0; j < sigma.size(); ++j) {
            if(sigma[j].is_redundant) continue;
            add_derived_binary(sigma[i].lhs, sigma[i].rhs, sigma[j].lhs, sigma[j].rhs, derived_lhs_pass, derived_rhs_pass, use_pruning, metrics, n_attributes);
        }
    }

    if (!derived_lhs_pass.empty()) {
        for (size_t k = 0; k < derived_lhs_pass.size(); ++k) {
            const Bitset& G = derived_lhs_pass[k];
            Bitset H = derived_rhs_pass[k];
            
            Bitset closure_G = calculate_pi_operator_binary(G, sigma, metrics);
            H &= ~closure_G;
            if (H.none()) continue;
            
            auto it = lhs_map.find(G);
            if (it == lhs_map.end()) {
                lhs_map[G] = sigma.size();
                sigma.push_back({G, H, false});
            } else {
                int existing_idx = it->second;
                sigma[existing_idx].rhs |= H;
                sigma[existing_idx].is_redundant = false;
            }
        }
    }
}

std::vector<SigmaRule> internal_run_lexicographic_binary(std::vector<SigmaRule> sigma_in, BinaryMetrics& metrics, bool use_pruning, int n_attributes) {
    std::map<Bitset, int> lhs_map;
    for(size_t i=0; i<sigma_in.size(); ++i) {
        lhs_map[sigma_in[i].lhs] = i;
    }

    while(true) {
        metrics.iterations++;
        std::vector<SigmaRule> sigma_old = sigma_in;
        
        saturate_system_lexicographic_binary(sigma_in, lhs_map, metrics, use_pruning, n_attributes);
        prune_system_binary(sigma_in, metrics);
        
        // Rebuild map after prune because indices or redundancy might have changed
        lhs_map.clear();
        for(size_t i=0; i<sigma_in.size(); ++i) {
            if (!sigma_in[i].is_redundant) lhs_map[sigma_in[i].lhs] = i;
        }

        bool same = true;
        if(sigma_in.size() == sigma_old.size()) {
            for(size_t i = 0; i < sigma_in.size(); ++i) {
                if(sigma_in[i].lhs != sigma_old[i].lhs || sigma_in[i].rhs != sigma_old[i].rhs || sigma_in[i].is_redundant != sigma_old[i].is_redundant) {
                    same = false; break;
                }
            }
        } else {
            same = false;
        }
        if(same) break;
    }
    return sigma_in;
}


} // anonymous namespace

// =============================================================================
// --- WRAPPERS EXPORTADOS ---
// =============================================================================

// [[Rcpp::export]]
List run_binary_monotonic_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_monotonic_binary(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;

    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

// [[Rcpp::export]]
List run_binary_dosp_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_dosp_binary(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;

    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

// [[Rcpp::export]]
List run_binary_single_pass_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_single_pass_binary(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;

    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

// [[Rcpp::export]]
List run_binary_monotonic_batch_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_monotonic_batch_bitwise(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;

    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

// [[Rcpp::export]]
List run_binary_tree_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_tree_binary(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;
    
    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

// [[Rcpp::export]]
List run_binary_lexicographic_optimized(S4 lhs_in, S4 rhs_in, bool use_pruning = true) {
    auto start_time = std::chrono::high_resolution_clock::now();
    BinaryMetrics metrics;
    int n_attributes = 0;
    
    std::vector<SigmaRule> sigma_in = Sparse_to_BinarySigma(lhs_in, rhs_in, n_attributes);
    std::vector<SigmaRule> sigma_final = internal_run_lexicographic_binary(sigma_in, metrics, use_pruning, n_attributes);

    metrics.execution_time = std::chrono::duration<double>(std::chrono::high_resolution_clock::now() - start_time).count();
    
    int active_rules = 0;
    for(const auto& r : sigma_final) { if(!r.is_redundant) active_rules++; }
    metrics.final_implication_count = active_rules;
    
    return List::create(
        Named("Sigma") = BinarySigma_to_R(sigma_final, n_attributes),
        Named("metrics") = Metrics_to_R(metrics)
    );
}

