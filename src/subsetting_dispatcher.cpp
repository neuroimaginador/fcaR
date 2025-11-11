#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// ===================================================================
// KERNEL 1: IGUALDAD DIFUSA O(N) (Tu lógica 'legacy' de 82ms)
// ===================================================================
inline int kernel_equal_fuzzy_O_N(const Rcpp::IntegerVector& X_i, const Rcpp::NumericVector& X_x, int X_start, int X_end,
                                  const Rcpp::IntegerVector& Y_i, const Rcpp::NumericVector& Y_x, int Y_start) {
  int pX = X_start;
  int pY = Y_start;
  int nnz = X_end - X_start;

  for (int p = 0; p < nnz; ++p) {
    if (X_i[pX + p] != Y_i[pY + p] || X_x[pX + p] != Y_x[pY + p]) {
      return -1; // No son iguales
    }
  }
  return 0; // Son iguales
}

// ===================================================================
// KERNEL 2: SUBCONJUNTO DIFUSO O(N+M) (Mi lógica 'merge' de 1095ms)
// ===================================================================
inline int kernel_subset_fuzzy_O_NplusM(const Rcpp::IntegerVector& X_i, const Rcpp::NumericVector& X_x, int X_start, int X_end,
                                        const Rcpp::IntegerVector& Y_i, const Rcpp::NumericVector& Y_x, int Y_start, int Y_end) {
  int pX = X_start;
  int pY = Y_start;

  if (pX == X_end) {
    return (pY == Y_end) ? 0 : 1;
  }
  bool is_proper = false;
  while (pX < X_end) {
    if (pY == Y_end) return -1;
    if (X_i[pX] < Y_i[pY]) {
      return -1;
    }
    if (X_i[pX] > Y_i[pY]) {
      is_proper = true;
      pY++;
      continue;
    }
    if (X_x[pX] > Y_x[pY]) {
      return -1;
    }
    if (X_x[pX] < Y_x[pY]) {
      is_proper = true;
    }
    pX++;
    pY++;
  }
  if (pY < Y_end) {
    is_proper = true;
  }
  return is_proper ? 1 : 0;
}

// ===================================================================
// KERNEL 3: BINARIO O(N/64) (Lógica de Bitset)
// ===================================================================
using NativeBitset = std::vector<uint64_t>;

void create_native_bitset(NativeBitset& out_cols, int num_rows, int num_cols,
                          const Rcpp::IntegerVector& p, const Rcpp::IntegerVector& i) {
  const size_t N_BLOCKS_N = (num_rows + 63) / 64;
  out_cols.assign(num_cols * N_BLOCKS_N, 0);
  for (int j = 0; j < num_cols; ++j) {
    int start = p[j];
    int end = p[j+1];
    for (int k = start; k < end; ++k) {
      int r = i[k];
      int block_idx = r / 64;
      int bit_idx = r % 64;
      out_cols[j * N_BLOCKS_N + block_idx] |= (1ULL << bit_idx);
    }
  }
}

inline int kernel_compare_bitset_O_N64(const uint64_t* X_ptr,
                                       const uint64_t* Y_ptr,
                                       const size_t N_BLOCKS_N) {
  bool is_proper = false;
  for (size_t b = 0; b < N_BLOCKS_N; ++b) {
    uint64_t Xk = X_ptr[b];
    uint64_t Yk = Y_ptr[b];
    if ((Xk & ~Yk) != 0) {
      return -1; // No es subconjunto
    }
    if ((~Xk & Yk) != 0) {
      is_proper = true; // Es subconjunto propio
    }
  }
  return is_proper ? 1 : 0;
}

// ===================================================================
// DISPATCHER C++ UNIFICADO
// ===================================================================
/**
 * @param proper_code 0=Equal, 1=Proper Subset, 2=Subset or Equal
 * @param is_binary Flag para seleccionar el kernel
 */
// [[Rcpp::export]]
List sparse_subset_dispatch(const Rcpp::IntegerVector& X_p, const Rcpp::IntegerVector& X_i, const Rcpp::NumericVector& X_x,
                            const Rcpp::IntegerVector& Y_p, const Rcpp::IntegerVector& Y_i, const Rcpp::NumericVector& Y_x,
                            int num_rows, int proper_code, bool is_binary) {

  int n_cols_X = X_p.size() - 1;
  int n_cols_Y = Y_p.size() - 1;

  std::vector<int> out_i;
  Rcpp::IntegerVector out_p(n_cols_X + 1);
  out_p[0] = 0;

  // --- RAMA BINARIA (BITSET O(N/64)) ---
  if (is_binary) {
    const size_t N_BLOCKS_N = (num_rows + 63) / 64;
    NativeBitset X_cols_flat;
    NativeBitset Y_cols_flat;
    create_native_bitset(X_cols_flat, num_rows, n_cols_X, X_p, X_i);
    create_native_bitset(Y_cols_flat, num_rows, n_cols_Y, Y_p, Y_i);

    for (int j = 0; j < n_cols_X; ++j) {
      const uint64_t* X_ptr = &X_cols_flat[j * N_BLOCKS_N];
      for (int k = 0; k < n_cols_Y; ++k) {
        const uint64_t* Y_ptr = &Y_cols_flat[k * N_BLOCKS_N];

        int result = kernel_compare_bitset_O_N64(X_ptr, Y_ptr, N_BLOCKS_N);

        bool match = false;
        if (proper_code == 0 && result == 0) match = true;
        else if (proper_code == 1 && result == 1) match = true;
        else if (proper_code == 2 && (result == 0 || result == 1)) match = true;

        if (match) out_i.push_back(k);
      }
      out_p[j+1] = out_i.size();
    }

  } else {
    // --- RAMA DIFUSA (O(N) o O(N+M)) ---
    for (int j = 0; j < n_cols_X; ++j) {
      int X_start = X_p[j];
      int X_end = X_p[j+1];
      int nnz_X = X_end - X_start;

      for (int k = 0; k < n_cols_Y; ++k) {
        int Y_start = Y_p[k];
        int Y_end = Y_p[k+1];
        int nnz_Y = Y_end - Y_start;

        bool match = false;

        if (proper_code == 0) { // --- IGUALDAD (Kernel O(N)) ---
          if (nnz_X != nnz_Y) continue;
          if (kernel_equal_fuzzy_O_N(X_i, X_x, X_start, X_end, Y_i, Y_x, Y_start) == 0) {
            match = true;
          }
        } else { // --- SUBCONJUNTO (Kernel O(N+M)) ---
          if (proper_code == 1) {
            if (nnz_X >= nnz_Y) continue;
          } else {
            if (nnz_X > nnz_Y) continue;
          }

          int result = kernel_subset_fuzzy_O_NplusM(X_i, X_x, X_start, X_end,
                                                    Y_i, Y_x, Y_start, Y_end);

          if (proper_code == 1 && result == 1) match = true;
          else if (proper_code == 2 && (result == 0 || result == 1)) match = true;
        }

        if (match) out_i.push_back(k);
      }
      out_p[j+1] = out_i.size();
    }
  }

  return List::create(_["i"] = Rcpp::wrap(out_i),
                      _["p"] = out_p);
}
