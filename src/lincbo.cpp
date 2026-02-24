#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include "fcaR_bitset.h"

using namespace Rcpp;

// =============================================================================
// --- LinCbO (Algorithm 8 & 9) - High-Performance Implementation ---
// Uses the shared fcaR::FastBitset from fcaR_bitset.h to avoid symbol collisions.
// =============================================================================

namespace {

using Bitset = fcaR::FastBitset;

struct LRule {
  Bitset lhs, rhs;
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
  std::vector<Bitset> attr_data;
  std::vector<Bitset> obj_data;

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
    count_stack.reserve(2 * nA * nA * 100);
  }

  inline void formal_closure(const Bitset &A, Bitset &res, Bitset &ext) const {
    ext.init(nO);
    ext.set();
    for (size_t j = A.find_first(); j != Bitset::npos && j < (size_t)nA; j = A.find_next(j)) {
      ext &= attr_data[j];
    }
    
    res.init(nA);
    if (ext.none()) {
      res.set();
    } else {
      res.set();
      for (size_t i = ext.find_first(); i != Bitset::npos && i < (size_t)nO; i = ext.find_next(i)) {
        res &= obj_data[i];
      }
    }
  }

  // Uses flat count stack offsets
  bool lin_closure_rc(const Bitset &B, int y, const Bitset &Z_prime, int prev_offset, int cur_offset, Bitset &D) {
    D = B;
    int old_size = (prev_offset >= 0) ? (cur_offset - prev_offset) : 0;
    int new_size = T.size();
    
    // Copy the previous count block
    if (old_size > 0) {
      std::copy_n(count_stack.begin() + prev_offset, old_size, count_stack.begin() + cur_offset);
    }
    
    Bitset B_old = B;
    B_old.bitwise_and_not(Z_prime);
    
    // Process new rules
    for (int r = old_size; r < new_size; ++r) {
      Bitset diff = T[r].lhs;
      diff.bitwise_and_not(B_old);
      count_stack[cur_offset + r] = diff.count();
    }

    Bitset Z = Z_prime;
    while (Z.any()) {
      size_t m_pos = Z.find_first();
      if (m_pos == Bitset::npos) break;
      int m = static_cast<int>(m_pos);
      Z.reset(m);
      
      // Traverse flattened linked list
      int curr = list_m.head[m];
      while (curr != -1) {
        int r = list_m.val[curr];
        if (--count_stack[cur_offset + r] == 0) {
          Bitset add = T[r].rhs;
          add.bitwise_and_not(D);
          if (add.any()) {
            size_t min_add_pos = add.find_first();
            int min_add = (min_add_pos != Bitset::npos) ? static_cast<int>(min_add_pos) : nA;
            if (y >= 0 && min_add < y) {
               return false;
            }
            D |= add;
            Z |= add;
          }
        }
        curr = list_m.next[curr];
      }
    }
    return true;
  }

  void step(Bitset B, int y, Bitset Z, int prev_offset) {
    if (++step_count % 4096 == 0) Rcpp::checkUserInterrupt();

    Bitset B_star;
    B_star.init(nA);
    
    int cur_offset = count_stack.size();
    // Expand count_stack for current depth
    count_stack.resize(cur_offset + T.size(), 0);
    
    if (!lin_closure_rc(B, y, Z, prev_offset, cur_offset, B_star)) {
      count_stack.resize(cur_offset); // pop
      return;
    }

    Bitset B_pp, ext;
    formal_closure(B_star, B_pp, ext);

    if (B_star != B_pp) {
      int r_idx = T.size();
      
      LRule new_rule;
      new_rule.lhs = B_star;
      new_rule.rhs = B_pp;
      T.push_back(std::move(new_rule));
      
      for (size_t i = B_star.find_first(); i != Bitset::npos && i < (size_t)nA; i = B_star.find_next(i)) {
        list_m.push_back(static_cast<int>(i), r_idx);
      }

      FinalRule fr;
      for (size_t k = B_star.find_first(); k != Bitset::npos && k < (size_t)nA; k = B_star.find_next(k)) {
        fr.lhs.push_back(static_cast<int>(k));
      }
      Bitset diff = B_pp;
      diff.bitwise_and_not(B_star);
      for (size_t k = diff.find_first(); k != Bitset::npos && k < (size_t)nA; k = diff.find_next(k)) {
        fr.rhs.push_back(static_cast<int>(k));
      }
      results.push_back(std::move(fr));

      count_stack.push_back(0);

      size_t min_diff_pos = diff.find_first();
      int min_diff = (min_diff_pos != Bitset::npos) ? static_cast<int>(min_diff_pos) : nA;
      if (y == -1 || min_diff >= y) {
        step(B_pp, y, diff, cur_offset);
      }
    } else {
      if (save_concepts) {
        FinalConcept fc;
        for (size_t k = B_star.find_first(); k != Bitset::npos && k < (size_t)nA; k = B_star.find_next(k)) {
          fc.intent.push_back(static_cast<int>(k));
        }
        for (size_t k = ext.find_first(); k != Bitset::npos && k < (size_t)nO; k = ext.find_next(k)) {
          fc.extent.push_back(static_cast<int>(k));
        }
        concepts.push_back(std::move(fc));
      }
      for (int i = nA - 1; i > y; --i) {
        if (!B_star.test(i)) {
          Bitset Bn = B_star;
          Bn.set(i);
          Bitset zn;
          zn.init(nA);
          zn.set(i);
          step(Bn, i, zn, cur_offset);
        }
      }
    }
    count_stack.resize(cur_offset); // pop
  }

  List solve() {
    Bitset B;
    B.init(nA);
    Bitset Z;
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
