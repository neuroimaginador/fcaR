#include <Rcpp.h>
#include <vector>
#include <queue>
#include <algorithm>
#include <set>

// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;

// =============================================================================
// 1. DILWORTH'S WIDTH (via Maximum Bipartite Matching)
// =============================================================================

bool dfs_matching(int u, const std::vector<std::vector<int>>& adj, 
                 std::vector<int>& matchR, std::vector<bool>& vis) {
  for (int v : adj[u]) {
    if (!vis[v]) {
      vis[v] = true;
      if (matchR[v] < 0 || dfs_matching(matchR[v], adj, matchR, vis)) {
        matchR[v] = u;
        return true;
      }
    }
  }
  return false;
}

// [[Rcpp::export]]
int calculate_width_cpp(IntegerVector i_idx, IntegerVector p_idx, int n) {
  std::vector<std::vector<int>> bipartite_adj(n);
  for (int j = 0; j < n; ++j) {
    for (int k = p_idx[j]; k < p_idx[j+1]; ++k) {
      int i = i_idx[k];
      if (i != j) {
        bipartite_adj[i].push_back(j);
      }
    }
  }
  
  std::vector<int> matchR(n, -1);
  int matching_size = 0;
  for (int i = 0; i < n; ++i) {
    std::vector<bool> vis(n, false);
    if (dfs_matching(i, bipartite_adj, matchR, vis)) {
      matching_size++;
    }
  }
  return n - matching_size;
}

// =============================================================================
// 2. ORDER DIMENSION (Greedy Linear Extension Discovery)
// =============================================================================

struct Pair {
  int x, y;
};

// [[Rcpp::export]]
int calculate_dimension_heuristic_cpp(IntegerVector i_idx, IntegerVector p_idx, int n) {
  if (n <= 1) return (n == 0) ? 0 : 1;
  
  // 1. Build Reachability Matrix (densify)
  std::vector<bool> reach_orig(n * n, false);
  for (int j = 0; j < n; ++j) {
    for (int k = p_idx[j]; k < p_idx[j+1]; ++k) {
      reach_orig[j * n + i_idx[k]] = true; // reach[col][row] -> row <= col
    }
  }
  
  // 2. Find Incomparable Pairs
  std::vector<Pair> incomparable;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j) {
      if (i == j) continue;
      if (!reach_orig[j * n + i] && !reach_orig[i * n + j]) {
        incomparable.push_back({i, j});
      }
    }
  }
  
  if (incomparable.empty()) return 1;
  
  // 3. Greedy Realizer Algorithm
  int dimension = 0;
  std::vector<bool> is_covered(incomparable.size(), false);
  int remaining = incomparable.size();
  
  while (remaining > 0 && dimension < n) {
    dimension++;
    std::vector<bool> current_reach = reach_orig;
    
    // Attempt to cover as many pairs as possible in this extension
    for (size_t k = 0; k < incomparable.size(); ++k) {
      if (is_covered[k]) continue;
      
      int u = incomparable[k].x;
      int v = incomparable[k].y;
      
      // Can we add u < v without a cycle?
      // Only if v </= u in current reach.
      if (!current_reach[u * n + v]) {
        // Add u < v and its transitive closure
        // New reach R' is: x R' y if (x R y) OR (x R u AND v R y)
        std::vector<bool> next_reach = current_reach;
        bool changed = false;
        
        // Optimized transitive closure update for adding one edge (u, v)
        for(int x = 0; x < n; ++x) {
          if (current_reach[u * n + x]) { // x <= u
            for(int y = 0; y < n; ++y) {
              if (current_reach[y * n + v]) { // v <= y
                if (!current_reach[y * n + x]) {
                  next_reach[y * n + x] = true;
                  changed = true;
                }
              }
            }
          }
        }
        
        // Safety check for cycle (x < x)
        bool cycle = false;
        for(int i=0; i<n; ++i) if(next_reach[i*n+i] && !reach_orig[i*n+i]) { cycle = true; break; }
        
        if (!cycle) {
          current_reach = next_reach;
          is_covered[k] = true;
          remaining--;
          
          // Also check if the reverse pair (v, u) was incomparable and now covered
          // (Flipping u < v implies v </= u in this extension)
        }
      }
    }
  }
  
  return dimension;
}
