#include <Rcpp.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// --- HELPER: Get indices of 1s and 0s ---
  // Returns a list of (row, col) pairs for 1s and 0s to speed up sampling
struct Coordinates {
  std::vector<int> rows;
  std::vector<int> cols;
};

// [[Rcpp::export]]
IntegerMatrix randomize_swap_cpp(IntegerMatrix I, int iterations) {
  // Clone to avoid modifying original
  IntegerMatrix Res = clone(I);
  int n_rows = Res.nrow();
  int n_cols = Res.ncol();

  // Locate all 1s to sample from efficiently
  std::vector<int> ones_idx; // Linear indices
  for (int i = 0; i < n_rows * n_cols; ++i) {
    if (Res[i] == 1) ones_idx.push_back(i);
  }

  int n_ones = ones_idx.size();
  if (n_ones < 2) return Res; // Cannot swap with less than 2 ones

  int swapped_count = 0;
  int attempts = 0;
  int max_attempts = iterations * 10; // Safety break

  while (swapped_count < iterations && attempts < max_attempts) {
    attempts++;

    // 1. Pick two random edges (1s): (r1, c1) and (r2, c2)
    int idx1_pos = (int)(R::runif(0, n_ones));
    int idx2_pos = (int)(R::runif(0, n_ones));

    if (idx1_pos == idx2_pos) continue;

    int linear1 = ones_idx[idx1_pos];
    int linear2 = ones_idx[idx2_pos];

    int r1 = linear1 % n_rows;
    int c1 = linear1 / n_rows;

    int r2 = linear2 % n_rows;
    int c2 = linear2 / n_rows;

    // Check if they form a swappable submatrix:
      // 1 0
    // 0 1
    // or
    // 0 1
    // 1 0
    // Target: (r1, c1)=1, (r2, c2)=1. We need (r1, c2)=0 and (r2, c1)=0

    if (r1 == r2 || c1 == c2) continue; // Same row or col, cannot swap

    if (Res(r1, c2) == 0 && Res(r2, c1) == 0) {
      // PERFOM SWAP
      // Remove (r1, c1), (r2, c2)
      Res(r1, c1) = 0;
      Res(r2, c2) = 0;

      // Add (r1, c2), (r2, c1)
      Res(r1, c2) = 1;
      Res(r2, c1) = 1;

      // Update index cache (Crucial for speed)
      // We replace the old linear indices with the new ones in our list
      // New linear indices:
        int new_linear1 = c2 * n_rows + r1;
      int new_linear2 = c1 * n_rows + r2;

      ones_idx[idx1_pos] = new_linear1;
      ones_idx[idx2_pos] = new_linear2;

      swapped_count++;
    }
  }

  return Res;
}

// [[Rcpp::export]]
IntegerMatrix randomize_rewire_cpp(IntegerMatrix I, int iterations) {
  IntegerMatrix Res = clone(I);
  int n_rows = Res.nrow();
  int n_cols = Res.ncol();

  std::vector<int> ones_idx;
  std::vector<int> zeros_idx;

  for (int i = 0; i < n_rows * n_cols; ++i) {
    if (Res[i] == 1) ones_idx.push_back(i);
    else zeros_idx.push_back(i);
  }

  int n_ones = ones_idx.size();
  int n_zeros = zeros_idx.size();

  if (n_ones == 0 || n_zeros == 0) return Res;

  for (int k = 0; k < iterations; ++k) {
    // Pick a 1 to remove
    int idx1_pos = (int)(R::runif(0, n_ones));
    int linear_one = ones_idx[idx1_pos];

    // Pick a 0 to add
    int idx0_pos = (int)(R::runif(0, n_zeros));
    int linear_zero = zeros_idx[idx0_pos];

    // Swap values
    Res[linear_one] = 0;
    Res[linear_zero] = 1;

    // Update cache
    ones_idx[idx1_pos] = linear_zero;
    zeros_idx[idx0_pos] = linear_one;
  }

  return Res;
}
