#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace Rcpp;

namespace {

struct TrieNode {
  int attr;
  int first_child;
  int next_sibling;
  std::vector<int> rule_ids;
};

} // namespace

//' @title Check equality of two implication sets using a Prefix Tree (Trie)
//' @description Tests if two sets of implications (given by their sparse LHS and RHS matrices)
//' are syntactically identical, regardless of the order of the implications.
//' @param lhs1 Sparse matrix (dgCMatrix) of LHS for set 1.
//' @param rhs1 Sparse matrix (dgCMatrix) of RHS for set 1.
//' @param lhs2 Sparse matrix (dgCMatrix) of LHS for set 2.
//' @param rhs2 Sparse matrix (dgCMatrix) of RHS for set 2.
//' @return Logical TRUE if sets are identical, FALSE otherwise.
//' @noRd
// [[Rcpp::export]]
bool check_equal_implications_trie_cpp(S4 lhs1, S4 rhs1, S4 lhs2, S4 rhs2) {
  IntegerVector dim1 = lhs1.slot("Dim");
  IntegerVector dim2 = lhs2.slot("Dim");
  
  int n_attrs1 = dim1[0];
  int n_rules1 = dim1[1];
  int n_attrs2 = dim2[0];
  int n_rules2 = dim2[1];
  
  if (n_rules1 != n_rules2) {
    return false;
  }
  if (n_attrs1 != n_attrs2) {
    return false;
  }
  if (n_rules1 == 0) {
    return true;
  }
  
  IntegerVector i_lhs1 = lhs1.slot("i");
  IntegerVector p_lhs1 = lhs1.slot("p");
  NumericVector x_lhs1 = lhs1.slot("x");
  
  IntegerVector i_rhs1 = rhs1.slot("i");
  IntegerVector p_rhs1 = rhs1.slot("p");
  NumericVector x_rhs1 = rhs1.slot("x");
  
  IntegerVector i_lhs2 = lhs2.slot("i");
  IntegerVector p_lhs2 = lhs2.slot("p");
  NumericVector x_lhs2 = lhs2.slot("x");
  
  IntegerVector i_rhs2 = rhs2.slot("i");
  IntegerVector p_rhs2 = rhs2.slot("p");
  NumericVector x_rhs2 = rhs2.slot("x");
  
  std::vector<TrieNode> nodes;
  nodes.reserve(n_rules1 * 4 + 1);
  nodes.push_back({-1, -1, -1, {}});
  
  for (int col = 0; col < n_rules1; ++col) {
    int curr = 0;
    int start = p_lhs1[col];
    int end = p_lhs1[col + 1];
    
    for (int k = start; k < end; ++k) {
      int a = i_lhs1[k];
      int child = nodes[curr].first_child;
      int prev_sibling = -1;
      bool found = false;
      
      while (child != -1) {
        if (nodes[child].attr == a) {
          curr = child;
          found = true;
          break;
        } else if (nodes[child].attr > a) {
          break;
        }
        prev_sibling = child;
        child = nodes[child].next_sibling;
      }
      
      if (!found) {
        int new_idx = static_cast<int>(nodes.size());
        nodes.push_back({a, -1, child, {}});
        if (prev_sibling == -1) {
          nodes[curr].first_child = new_idx;
        } else {
          nodes[prev_sibling].next_sibling = new_idx;
        }
        curr = new_idx;
      }
    }
    nodes[curr].rule_ids.push_back(col);
  }
  
  std::vector<bool> matched(n_rules1, false);
  
  for (int col2 = 0; col2 < n_rules2; ++col2) {
    int curr = 0;
    int start2 = p_lhs2[col2];
    int end2 = p_lhs2[col2 + 1];
    int len_lhs2 = end2 - start2;
    
    bool path_exists = true;
    for (int k = start2; k < end2; ++k) {
      int a = i_lhs2[k];
      int child = nodes[curr].first_child;
      bool found = false;
      
      while (child != -1) {
        if (nodes[child].attr == a) {
          curr = child;
          found = true;
          break;
        } else if (nodes[child].attr > a) {
          break;
        }
        child = nodes[child].next_sibling;
      }
      
      if (!found) {
        path_exists = false;
        break;
      }
    }
    
    if (!path_exists) {
      return false;
    }
    
    const std::vector<int>& candidate_rules = nodes[curr].rule_ids;
    int matched_r = -1;
    
    for (int r : candidate_rules) {
      if (matched[r]) continue;
      
      int start1 = p_lhs1[r];
      int end1 = p_lhs1[r + 1];
      if ((end1 - start1) != len_lhs2) continue;
      
      bool lhs_vals_match = true;
      for (int k = 0; k < len_lhs2; ++k) {
        if (std::abs(x_lhs1[start1 + k] - x_lhs2[start2 + k]) > 1e-9) {
          lhs_vals_match = false;
          break;
        }
      }
      if (!lhs_vals_match) continue;
      
      int start_rhs1 = p_rhs1[r];
      int end_rhs1 = p_rhs1[r + 1];
      int start_rhs2 = p_rhs2[col2];
      int end_rhs2 = p_rhs2[col2 + 1];
      
      if ((end_rhs1 - start_rhs1) != (end_rhs2 - start_rhs2)) continue;
      
      int len_rhs = end_rhs1 - start_rhs1;
      bool rhs_match = true;
      for (int k = 0; k < len_rhs; ++k) {
        if (i_rhs1[start_rhs1 + k] != i_rhs2[start_rhs2 + k]) {
          rhs_match = false;
          break;
        }
        if (std::abs(x_rhs1[start_rhs1 + k] - x_rhs2[start_rhs2 + k]) > 1e-9) {
          rhs_match = false;
          break;
        }
      }
      
      if (rhs_match) {
        matched_r = r;
        break;
      }
    }
    
    if (matched_r == -1) {
      return false;
    }
    matched[matched_r] = true;
  }
  
  return true;
}
