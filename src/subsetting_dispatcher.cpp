#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// ===================================================================
// KERNEL 1: IGUALDAD O(N) - VERSIÓN PUNTEROS CRUDOS
// ===================================================================
inline bool kernel_equal_raw(const int* Xi, const double* Xx, int nnz,
                             const int* Yi, const double* Yx) {
  for (int p = 0; p < nnz; ++p) {
    // Acceso directo a memoria, sin chequeo de límites de Rcpp
    if (Xi[p] != Yi[p] || Xx[p] != Yx[p]) {
      return false;
    }
  }
  return true;
}

// ===================================================================
// KERNEL 2: SUBCONJUNTO O(N+M) - VERSIÓN OPTIMIZADA CON PRE-CHECKS
// ===================================================================
inline int kernel_subset_raw(const int* Xi, const double* Xx, int nnz_X,
                             const int* Yi, const double* Yx, int nnz_Y) {

  // --- OPTIMIZACIÓN "BOUNDING BOX" O(1) ---
  // Como las matrices CSC tienen índices ordenados:
  // 1. Si el primer índice de X es menor que el primero de Y, X tiene algo que Y no.
  if (Xi[0] < Yi[0]) return -1;
  // 2. Si el último índice de X es mayor que el último de Y, imposible ser subset.
  if (Xi[nnz_X - 1] > Yi[nnz_Y - 1]) return -1;

  int pX = 0;
  int pY = 0;
  bool is_proper = false;

  while (pX < nnz_X) {
    // Si se acaba Y pero X sigue, imposible (ya cubierto arriba parcialmente, pero necesario)
    if (pY == nnz_Y) return -1;

    int idx_X = Xi[pX];
    int idx_Y = Yi[pY];

    if (idx_X < idx_Y) {
      return -1; // X tiene fila que Y no tiene
    } else if (idx_X > idx_Y) {
      is_proper = true; // Y tiene fila extra (potencial subset propio)
      pY++;
    } else {
      // Mismo índice, comparar valores
      if (Xx[pX] > Yx[pY]) return -1; // Valor de X excede Y
      if (Xx[pX] < Yx[pY]) is_proper = true; // Valor de X menor (subset)
      pX++;
      pY++;
    }
  }

  // Si terminamos X, y a Y le quedan elementos
  if (pY < nnz_Y) is_proper = true;

  return is_proper ? 1 : 0;
}

// ===================================================================
// KERNEL 3: BINARIO BITSET - VERSIÓN VECTOR PLANO
// ===================================================================
using NativeBitset = std::vector<uint64_t>;

// Crea el bitset en un solo vector plano para mejorar la caché
void create_native_bitset_raw(NativeBitset& out_cols, int num_rows, int num_cols,
                              const int* p, const int* i, size_t N_BLOCKS) {
  // Reservar e inicializar a 0 de golpe
  out_cols.assign(num_cols * N_BLOCKS, 0);

  for (int j = 0; j < num_cols; ++j) {
    int start = p[j];
    int end = p[j+1];
    // Puntero al inicio del bloque de esta columna
    uint64_t* col_ptr = &out_cols[j * N_BLOCKS];

    for (int k = start; k < end; ++k) {
      int r = i[k];
      col_ptr[r / 64] |= (1ULL << (r % 64));
    }
  }
}

inline int kernel_compare_bitset_raw(const uint64_t* X_ptr,
                                     const uint64_t* Y_ptr,
                                     size_t N_BLOCKS) {
  bool is_proper = false;

  for (size_t b = 0; b < N_BLOCKS; ++b) {
    uint64_t Xk = X_ptr[b];

    // --- OPTIMIZACIÓN DE CEROS ---
    // Si el bloque de X es 0, seguro es subset de lo que sea que tenga Y.
    // Solo comprobamos si Y aporta "propiedad" (es mayor que 0).
    if (Xk == 0) {
      if (Y_ptr[b] != 0) is_proper = true;
      continue;
    }

    uint64_t Yk = Y_ptr[b];

    // Chequeo estándar: bits en X que no están en Y
    if ((Xk & ~Yk) != 0) {
      return -1;
    }
    // Chequeo proper: bits en Y que no están en X
    if (!is_proper && ((~Xk & Yk) != 0)) {
      is_proper = true;
    }
  }
  return is_proper ? 1 : 0;
}

// ===================================================================
// DISPATCHER UNIFICADO OPTIMIZADO
// ===================================================================
// [[Rcpp::export]]
List sparse_subset_dispatch(const Rcpp::IntegerVector& X_p, const Rcpp::IntegerVector& X_i, const Rcpp::NumericVector& X_x,
                                const Rcpp::IntegerVector& Y_p, const Rcpp::IntegerVector& Y_i, const Rcpp::NumericVector& Y_x,
                                int num_rows, int proper_code, bool is_binary) {

  int n_cols_X = X_p.size() - 1;
  int n_cols_Y = Y_p.size() - 1;

  // Obtener punteros crudos (Read-Only) para evitar overhead de Rcpp dentro del loop
  const int* Xp_ptr = X_p.begin();
  const int* Xi_ptr = X_i.begin();
  const double* Xx_ptr = X_x.begin();

  const int* Yp_ptr = Y_p.begin();
  const int* Yi_ptr = Y_i.begin();
  const double* Yx_ptr = Y_x.begin();

  // Vectores de salida
  std::vector<int> out_i;
  out_i.reserve(n_cols_X); // Reserva mínima razonable
  Rcpp::IntegerVector out_p(n_cols_X + 1);
  out_p[0] = 0;

  if (is_binary) {
    // --- RAMA BINARIA ---
    size_t N_BLOCKS = (num_rows + 63) / 64;
    NativeBitset X_bits, Y_bits;

    // Crear bitsets usando punteros
    create_native_bitset_raw(X_bits, num_rows, n_cols_X, Xp_ptr, Xi_ptr, N_BLOCKS);
    create_native_bitset_raw(Y_bits, num_rows, n_cols_Y, Yp_ptr, Yi_ptr, N_BLOCKS);

    const uint64_t* X_bits_data = X_bits.data();
    const uint64_t* Y_bits_data = Y_bits.data();

    for (int j = 0; j < n_cols_X; ++j) {
      const uint64_t* curr_X_ptr = &X_bits_data[j * N_BLOCKS];

      for (int k = 0; k < n_cols_Y; ++k) {
        const uint64_t* curr_Y_ptr = &Y_bits_data[k * N_BLOCKS];

        int result = kernel_compare_bitset_raw(curr_X_ptr, curr_Y_ptr, N_BLOCKS);

        bool match = false;
        if (proper_code == 0 && result == 0) match = true;
        else if (proper_code == 1 && result == 1) match = true;
        else if (proper_code == 2 && result >= 0) match = true;

        if (match) out_i.push_back(k);
      }
      out_p[j+1] = out_i.size();
    }

  } else {
    // --- RAMA DIFUSA (O(N) / O(N+M)) ---
    for (int j = 0; j < n_cols_X; ++j) {
      int X_start = Xp_ptr[j];
      int nnz_X   = Xp_ptr[j+1] - X_start;
      // Punteros al inicio de la columna j
      const int* cur_Xi = &Xi_ptr[X_start];
      const double* cur_Xx = &Xx_ptr[X_start];

      for (int k = 0; k < n_cols_Y; ++k) {
        int Y_start = Yp_ptr[k];
        int nnz_Y   = Yp_ptr[k+1] - Y_start;

        bool match = false;

        if (proper_code == 0) {
          // --- IGUALDAD ---
          if (nnz_X == nnz_Y) {
            const int* cur_Yi = &Yi_ptr[Y_start];
            const double* cur_Yx = &Yx_ptr[Y_start];
            if (kernel_equal_raw(cur_Xi, cur_Xx, nnz_X, cur_Yi, cur_Yx)) {
              match = true;
            }
          }
        } else {
          // --- SUBCONJUNTO ---
          // Pre-filtros rápidos de conteo (Cardinalidad)
          if (proper_code == 1 && nnz_X >= nnz_Y) continue;
          if (proper_code == 2 && nnz_X > nnz_Y) continue;

          // Caso especial: X vacío es subset de todo (asumiendo valores >=0)
          if (nnz_X == 0) {
            if (proper_code == 2) match = true;
            else if (proper_code == 1 && nnz_Y > 0) match = true;
          } else {
            const int* cur_Yi = &Yi_ptr[Y_start];
            const double* cur_Yx = &Yx_ptr[Y_start];

            int result = kernel_subset_raw(cur_Xi, cur_Xx, nnz_X, cur_Yi, cur_Yx, nnz_Y);

            if (proper_code == 1 && result == 1) match = true;
            else if (proper_code == 2 && result >= 0) match = true;
          }
        }

        if (match) out_i.push_back(k);
      }
      out_p[j+1] = out_i.size();
    }
  }

  return List::create(_["i"] = Rcpp::wrap(out_i),
                      _["p"] = out_p);
}
