#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// Helper: Check if sparse vector A is a subset of B
// A_ptr, B_ptr: pointers to row indices in the 'i' slot
// A_len, B_len: number of non-zero elements
bool is_subset_sparse(const int* A_ptr, int A_len, const int* B_ptr, int B_len) {
  int i = 0, j = 0;
  while (i < A_len && j < B_len) {
    if (A_ptr[i] < B_ptr[j]) return false; // A has an element not in B
    if (A_ptr[i] == B_ptr[j]) i++;         // Match found
    j++;
  }
  return (i == A_len);
}

// [[Rcpp::export]]
NumericVector calculate_stability_sparse_rcpp(S4 mat) {
  IntegerVector i_slot = mat.slot("i");
  IntegerVector p_slot = mat.slot("p");
  IntegerVector Dim = mat.slot("Dim");
  int n_concepts = Dim[1];

  const int* i_ptr = i_slot.begin();
  const int* p_ptr = p_slot.begin();

  std::vector<int> sizes(n_concepts);
  std::vector<int> order(n_concepts);
  NumericVector stability(n_concepts, 1.0);

  for (int k = 0; k < n_concepts; ++k) {
    sizes[k] = p_ptr[k+1] - p_ptr[k];
    order[k] = k;
  }

  // Sort by size to process subconcepts first (Bottom-Up)
  std::sort(order.begin(), order.end(), [&](int a, int b) {
    return sizes[a] < sizes[b];
  });

  for (int k = 0; k < n_concepts; ++k) {
    int idx_sub = order[k];
    int size_sub = sizes[idx_sub];
    const int* sub_indices = i_ptr + p_ptr[idx_sub];

    for (int m = k + 1; m < n_concepts; ++m) {
      int idx_super = order[m];
      int size_super = sizes[idx_super];

      if (size_sub == size_super) continue;

      const int* super_indices = i_ptr + p_ptr[idx_super];

      if (is_subset_sparse(sub_indices, size_sub, super_indices, size_super)) {
        double diff = (double)size_sub - (double)size_super;
        double factor = std::pow(2.0, diff);
        stability[idx_super] -= stability[idx_sub] * factor;
      }
    }
  }

  for(int k=0; k<n_concepts; ++k) {
    if(stability[k] < 0) stability[k] = 0.0;
    if(stability[k] > 1) stability[k] = 1.0;
  }
  return stability;
}

// [[Rcpp::export]]
NumericVector calculate_density_rcpp(S4 extents, S4 intents, NumericMatrix I) {
  // Access sparse matrix slots for extents and intents
  IntegerVector ext_i = extents.slot("i");
  IntegerVector ext_p = extents.slot("p");
  IntegerVector ext_dim = extents.slot("Dim");

  IntegerVector int_i = intents.slot("i");
  IntegerVector int_p = intents.slot("p");

  int n_concepts = ext_dim[1];
  NumericVector density(n_concepts);

  const int* ext_rows = ext_i.begin();
  const int* ext_ptr = ext_p.begin();
  const int* int_rows = int_i.begin();
  const int* int_ptr = int_p.begin();

  int n_rows_I = I.nrow();

  for (int k = 0; k < n_concepts; ++k) {
    int n_obj = ext_ptr[k+1] - ext_ptr[k];
    int n_att = int_ptr[k+1] - int_ptr[k];

    if (n_obj == 0 || n_att == 0) {
      density[k] = 0.0;
      continue;
    }

    double sum_val = 0.0;

    // Iterate over the Cartesian product of Extent x Intent
    for (int o = ext_ptr[k]; o < ext_ptr[k+1]; ++o) {
      int obj_idx = ext_rows[o];
      for (int a = int_ptr[k]; a < int_ptr[k+1]; ++a) {
        int att_idx = int_rows[a];
        // R matrices are column-major: index = row + col * nrow
        sum_val += I[obj_idx + att_idx * n_rows_I];
      }
    }
    density[k] = sum_val / (double)(n_obj * n_att);
  }
  return density;
}
