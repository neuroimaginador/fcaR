#include <Rcpp.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;

// =============================================================================
// --- LINCBO (Algorithm 8 & 9) - DEEP MEMORY OPTIMIZED ---
// =============================================================================

namespace {

// Fast bitset with Small-Object Optimization (SBO)
// Stores up to 256 bits inline avoiding heap allocations for typical FCA contexts.
class FastBitset {
public:
  uint64_t inline_words[4];
  std::vector<uint64_t> heap_words;
  uint64_t* ptr;
  int n_words;
  int n_bits;

  FastBitset() : ptr(nullptr), n_words(0), n_bits(0) {}
  
  // Custom destructor, copy constructor, assignment operator for safety if needed,
  // but Rcpp vectors of objects often need valid defaults.
  // Using explicit init() is safer here.
  
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
      if (old_words > 4) std::vector<uint64_t>().swap(heap_words); // free
      ptr = inline_words;
      for (int i=0; i<n_words; ++i) inline_words[i] = other.ptr[i];
    } else {
      heap_words = other.heap_words;
      ptr = heap_words.data();
    }
    return *this;
  }
  
  // Fast move constructor to avoid copying arrays returned by value
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
      std::vector<uint64_t>().swap(heap_words); // free
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

  inline void bitwise_and(const FastBitset& other) {
    for (int i = 0; i < n_words; ++i) {
      ptr[i] &= other.ptr[i];
    }
  }

  inline void bitwise_or(const FastBitset& other) {
    for (int i = 0; i < n_words; ++i) {
      ptr[i] |= other.ptr[i];
    }
  }

  inline void bitwise_and_not(const FastBitset& other) {
    for (int i = 0; i < n_words; ++i) {
      ptr[i] &= ~other.ptr[i];
    }
  }

  inline bool none() const {
    for (int i = 0; i < n_words; ++i) if (ptr[i]) return false;
    return true;
  }

  inline bool any() const { return !none(); }

  inline bool is_subset_of(const FastBitset& other) const {
    for (int i=0; i<n_words; ++i) {
      if ((ptr[i] & other.ptr[i]) != ptr[i]) return false;
    }
    return true;
  }

  FastBitset& operator|=(const FastBitset& other) {
    for (int i=0; i<n_words; ++i) ptr[i] |= other.ptr[i];
    return *this;
  }

  FastBitset& operator&=(const FastBitset& other) {
    for (int i=0; i<n_words; ++i) ptr[i] &= other.ptr[i];
    return *this;
  }
  
  bool operator==(const FastBitset& other) const {
    if (n_bits != other.n_bits) return false;
    for (int i=0; i<n_words; ++i) if (ptr[i] != other.ptr[i]) return false;
    return true;
  }

  inline bool operator!=(const FastBitset& other) const {
    for (int i = 0; i < n_words; ++i) {
      if (ptr[i] != other.ptr[i]) return true;
    }
    return false;
  }

  inline int count() const {
    int c = 0;
    for (int i = 0; i < n_words; ++i) {
      c += __builtin_popcountll(ptr[i]);
    }
    return c;
  }

  inline int find_first() const {
    for (int i = 0; i < n_words; ++i) {
      if (ptr[i] != 0) {
        return (i << 6) + __builtin_ctzll(ptr[i]);
      }
    }
    return n_bits;
  }

  inline int find_next(int prev) const {
    int next_idx = prev + 1;
    if (next_idx >= n_bits) return n_bits;
    int word_idx = next_idx >> 6;
    uint64_t mask = ~0ULL << (next_idx & 63);
    uint64_t masked_word = ptr[word_idx] & mask;
    if (masked_word != 0) {
      return (word_idx << 6) + __builtin_ctzll(masked_word);
    }
    for (int i = word_idx + 1; i < n_words; ++i) {
      if (ptr[i] != 0) {
        return (i << 6) + __builtin_ctzll(ptr[i]);
      }
    }
    return n_bits;
  }
};

struct LRule {
  FastBitset lhs, rhs;
};

// Flattened Adjacency List for storing List[m] logic with zero vector overheads
class FlatListM {
public:
  std::vector<int> head;
  std::vector<int> next;
  std::vector<int> val;

  FlatListM(int num_attrs) {
    head.assign(num_attrs, -1);
  }

  inline void push_back(int m, int rule_idx) {
    int node_idx = val.size();
    val.push_back(rule_idx);
    next.push_back(head[m]);
    head[m] = node_idx;
  }
};

class LinCbOSolver {
public:
  int nO, nA;
  std::vector<FastBitset> attr_data;
  std::vector<FastBitset> obj_data;

  std::vector<LRule> T;
  FlatListM list_m;

  struct FinalRule {
    std::vector<int> lhs, rhs;
  };
  std::vector<FinalRule> results;

  struct FinalConcept {
    std::vector<int> intent, extent;
  };
  std::vector<FinalConcept> concepts;

  int step_count;
  bool save_concepts;
  
  // Single global count stack to avoid vector depth pools entirely
  std::vector<int> count_stack;

  LinCbOSolver(NumericMatrix I, bool sc) : nO(I.nrow()), nA(I.ncol()), list_m(I.ncol()), step_count(0), save_concepts(sc) {
    attr_data.resize(nA);
    obj_data.resize(nO);
    for (int j = 0; j < nA; ++j) attr_data[j].init(nO);
    for (int i = 0; i < nO; ++i) obj_data[i].init(nA);
    
    for (int i = 0; i < nO; i++) {
      for (int j = 0; j < nA; j++) {
        if (I(i, j) >= 1.0) {
          obj_data[i].set(j);
          attr_data[j].set(i);
        }
      }
    }
    // Preallocate extreme count stack depth conservatively to avoid reallocs
    count_stack.reserve(2 * nA * nA * 100);
  }

  inline void formal_closure(const FastBitset &A, FastBitset &res, FastBitset &ext) const {
    ext.init(nO);
    ext.set();
    for (int j = A.find_first(); j < nA; j = A.find_next(j)) {
      ext.bitwise_and(attr_data[j]);
    }
    
    res.init(nA);
    if (ext.none()) {
      res.set();
    } else {
      res.set();
      for (int i = ext.find_first(); i < nO; i = ext.find_next(i)) {
        res.bitwise_and(obj_data[i]);
      }
    }
  }

  // Uses flat count stack offsets
  bool lin_closure_rc(const FastBitset &B, int y, const FastBitset &Z_prime, int prev_offset, int cur_offset, FastBitset &D) {
    D = B;
    int old_size = (prev_offset >= 0) ? (cur_offset - prev_offset) : 0;
    int new_size = T.size();
    
    // Copy the previous count block
    if (old_size > 0) {
      std::copy_n(count_stack.begin() + prev_offset, old_size, count_stack.begin() + cur_offset);
    }
    
    FastBitset B_old = B;
    B_old.bitwise_and_not(Z_prime);
    
    // Process new rules
    for (int r = old_size; r < new_size; ++r) {
      FastBitset diff = T[r].lhs;
      diff.bitwise_and_not(B_old);
      count_stack[cur_offset + r] = diff.count();
    }

    FastBitset Z = Z_prime;
    while (!Z.none()) {
      int m = Z.find_first();
      Z.reset(m);
      
      // Traverse flattened linked list
      int curr = list_m.head[m];
      while (curr != -1) {
        int r = list_m.val[curr];
        if (--count_stack[cur_offset + r] == 0) {
          FastBitset add = T[r].rhs;
          add.bitwise_and_not(D);
          if (!add.none()) {
            int min_add = add.find_first();
            if (y >= 0 && min_add < y) {
               return false;
            }
            D.bitwise_or(add);
            Z.bitwise_or(add);
          }
        }
        curr = list_m.next[curr];
      }
    }
    return true;
  }

  void step(FastBitset B, int y, FastBitset Z, int prev_offset) {
    if (++step_count % 4096 == 0) Rcpp::checkUserInterrupt();

    FastBitset B_star;
    B_star.init(nA);
    
    int cur_offset = count_stack.size();
    // Expand count_stack for current depth
    count_stack.resize(cur_offset + T.size(), 0);
    
    if (!lin_closure_rc(B, y, Z, prev_offset, cur_offset, B_star)) {
      count_stack.resize(cur_offset); // pop
      return;
    }

    FastBitset B_pp, ext;
    formal_closure(B_star, B_pp, ext);

    if (B_star != B_pp) {
      int r_idx = T.size();
      
      LRule new_rule;
      new_rule.lhs.init(nA);
      new_rule.lhs = B_star;
      new_rule.rhs.init(nA);
      new_rule.rhs = B_pp;
      T.push_back(std::move(new_rule));
      
      for (int i = B_star.find_first(); i < nA; i = B_star.find_next(i)) {
        list_m.push_back(i, r_idx);
      }

      FinalRule fr;
      for (int k = B_star.find_first(); k < nA; k = B_star.find_next(k)) {
        fr.lhs.push_back(k);
      }
      FastBitset diff = B_pp;
      diff.bitwise_and_not(B_star);
      for (int k = diff.find_first(); k < nA; k = diff.find_next(k)) {
        fr.rhs.push_back(k);
      }
      results.push_back(std::move(fr));

      // Append rule threshold mapping (1 because new element added locally differs immediately by 0, but initialization dictates logic count)
      // lin_closure_rc recalculates immediately next descent
      count_stack.push_back(0); // Safe pad for deeper descents

      int min_diff = diff.none() ? nA : diff.find_first();
      if (y == -1 || min_diff >= y) {
        step(B_pp, y, diff, cur_offset);
      }
    } else {
      if (save_concepts) {
        FinalConcept fc;
        for (int k = B_star.find_first(); k < nA; k = B_star.find_next(k)) {
          fc.intent.push_back(k);
        }
        for (int k = ext.find_first(); k < nO; k = ext.find_next(k)) {
          fc.extent.push_back(k);
        }
        concepts.push_back(std::move(fc));
      }
      for (int i = nA - 1; i > y; --i) {
        if (!B_star.test(i)) {
          FastBitset Bn;
          Bn.init(nA);
          Bn = B_star;
          Bn.set(i);
          FastBitset zn;
          zn.init(nA);
          zn.set(i);
          step(Bn, i, zn, cur_offset);
        }
      }
    }
    count_stack.resize(cur_offset); // pop
  }

  List solve() {
    FastBitset B;
    B.init(nA);
    FastBitset Z;
    Z.init(nA);
    step(B, -1, Z, -1);

    std::vector<int> li, lp, ri, rp;
    lp.push_back(0);
    rp.push_back(0);
    for (const auto &m : results) {
      for (int v : m.lhs) li.push_back(v);
      lp.push_back(lp.back() + m.lhs.size());
      for (int v : m.rhs) ri.push_back(v);
      rp.push_back(rp.back() + m.rhs.size());
    }

    S4 L("dgCMatrix"), R("dgCMatrix");
    auto mk = [&](S4 &s, std::vector<int> &idx, std::vector<int> &p, int n_rows, int n_cols) {
      s.slot("i") = wrap(idx);
      s.slot("p") = wrap(p);
      s.slot("x") = NumericVector(idx.size(), 1.0);
      s.slot("Dim") = IntegerVector::create(n_rows, n_cols);
    };
    mk(L, li, lp, nA, results.size());
    mk(R, ri, rp, nA, results.size());
    
    if (save_concepts) {
      std::vector<int> ci, cp, ei, ep;
      cp.push_back(0);
      ep.push_back(0);
      for (const auto &c : concepts) {
        for (int v : c.intent) ci.push_back(v);
        cp.push_back(cp.back() + c.intent.size());
        for (int v : c.extent) ei.push_back(v);
        ep.push_back(ep.back() + c.extent.size());
      }
      S4 C("dgCMatrix"), E("dgCMatrix");
      mk(C, ci, cp, nA, concepts.size());
      mk(E, ei, ep, nO, concepts.size());
      return List::create(_["concepts"] = C, _["extents"] = E, _["LHS"] = L, _["RHS"] = R);
    }
    
    return List::create(_["LHS"] = L, _["RHS"] = R);
  }
};

} // anonymous namespace

// [[Rcpp::export]]
List binary_lincbo_implications(NumericMatrix I, bool save_concepts = false, bool verbose = false) {
  LinCbOSolver solver(I, save_concepts);
  return solver.solve();
}
