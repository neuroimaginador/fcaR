#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include "aux_functions.h"       // Dependencia de fcaR (para S4)
#include "vector_operations.h" // Dependencia de fcaR (para Structs C)

#include <vector>
#include <algorithm> // Para std::sort
#include <utility>   // Para std::pair
#include <cstring>   // Para memcpy
#include <cstdlib>   // Para realloc

using namespace Rcpp;

// =============================================================================
// --- HELPERS DE E/S A GRANEL ---
// =============================================================================
// (Versión estable v9 para Doubles)
inline void ensureArray_double_v9(DoubleArray *a, size_t additional_size) {
  // Si el array es NULL, lo inicializamos correctamente para realloc
  if (a->array == NULL) a->size = 0;

  if (a->used + additional_size > a->size) {
    size_t newSize = (a->size + additional_size) * 1.5;
    if (newSize == 0) newSize = 1;
    double* tmp = (double *)realloc(a->array, newSize * sizeof(double));
    if (tmp == NULL) { Rcpp::stop("Failed to realloc memory (double)"); }
    a->array = tmp; a->size = newSize;
    // Rellenamos la nueva memoria con 0
    for (size_t i = a->used; i < a->size; i++) { a->array[i] = 0; }
  }
}

inline void insertArray_bulk_double_v9(DoubleArray *a, double* buffer, size_t n) {
  ensureArray_double_v9(a, n);
  memcpy(&a->array[a->used], buffer, n * sizeof(double));
  a->used += n;
}

// (Versión estable v10 para Integers)
inline void ensureArray_int_v10(IntArray *a, size_t additional_size) {
  // Si el array es NULL, lo inicializamos
  if (a->array == NULL) a->size = 0;

  if (a->used + additional_size > a->size) {
    size_t newSize = (a->size + additional_size) * 1.5;
    if (newSize == 0) newSize = 1;
    int* tmp = (int *)realloc(a->array, newSize * sizeof(int));
    if (tmp == NULL) { Rcpp::stop("Failed to realloc memory (int)"); }
    a->array = tmp; a->size = newSize;
    for (size_t i = a->used; i < a->size; i++) { a->array[i] = 0; }
  }
}
// (Helper v13: memcpy directo desde std::vector<int>)
inline void insertArray_bulk_int_v13(IntArray *a, const int* buffer, size_t n) {
  ensureArray_int_v10(a, n);
  memcpy(&a->array[a->used], buffer, n * sizeof(int));
  a->used += n;
}
// --- Fin de Helpers ---


// =============================================================================
// --- VERSIÓN REORDER (v_transpose + Reordenación de Atributos) ---
// =============================================================================

using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>; // [bloque][objeto]
using IntentAccumulator_Reorder = std::vector<uint64_t>;

// --- Helpers Nativos v_reorder ---
inline bool test_bit_native_M_Reorder(const IntentAccumulator_Reorder& blocks, int k) {
  int block_idx = k / 64;
  int bit_idx = k % 64;
  if (block_idx >= blocks.size()) return false;
  return (blocks[block_idx] & (1ULL << bit_idx)) != 0;
}

inline bool test_bit_native_M_Reorder_flat(const uint64_t* blocks, int k) {
  int block_idx = k / 64;
  int bit_idx = k % 64;
  return (blocks[block_idx] & (1ULL << bit_idx)) != 0;
}

inline bool test_bit_native_N_Reorder(const AttributeCols_Reorder& blocks,
                                      int attr_j, int obj_idx,
                                      const size_t N_BLOCKS_N) {
  int block_idx = obj_idx / 64;
  int bit_idx = obj_idx % 64;
  return (blocks[attr_j * N_BLOCKS_N + block_idx] & (1ULL << bit_idx)) != 0;
}


void inclose_core_reorder(int y,
                          int n_objects,
                          int n_attributes,
                          const size_t N_BLOCKS_M,
                          const size_t N_BLOCKS_N,
                          Extent_Reorder& extent,
                          IntentAccumulator_Reorder& intent,
                          const AttributeCols_Reorder& attr_cols_flat,
                          const ObjectRows_Reorder& obj_rows_transposed,
                          std::vector<int>& ext_i_out,
                          std::vector<int>& ext_p_out,
                          std::vector<uint64_t>& int_blocks_out,
                          double* canonicity_tests) {

  ext_p_out.push_back(ext_i_out.size());
  ext_i_out.insert(ext_i_out.end(), extent.begin(), extent.end());
  int_blocks_out.insert(int_blocks_out.end(), intent.begin(), intent.end());

  Extent_Reorder child_extent;
  child_extent.reserve(extent.size());
  IntentAccumulator_Reorder child_intent(N_BLOCKS_M);

  for (int j = y + 1; j < n_attributes; j++) {
    if (test_bit_native_M_Reorder(intent, j)) continue;

    child_extent.clear();
    for (int obj_idx : extent) {
      if (test_bit_native_N_Reorder(attr_cols_flat, j, obj_idx, N_BLOCKS_N)) {
        child_extent.push_back(obj_idx);
      }
    }

    if (child_extent.empty()) continue;

    for(size_t k = 0; k < N_BLOCKS_M; ++k) {
      const uint64_t* row_k_ptr = &obj_rows_transposed[k * n_objects];
      uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
      for (int obj_idx : child_extent) {
        intent_k &= row_k_ptr[obj_idx];
      }
      child_intent[k] = intent_k;
    }

    (*canonicity_tests) += 1.0;
    bool is_canonical = true;
    const int j_block_idx = j / 64;
    const int j_bit_idx = j % 64;

    for (int b = 0; b < j_block_idx; b++) {
      if ((child_intent[b] & (~intent[b])) != 0) { is_canonical = false; break; }
    }
    if (is_canonical) {
      uint64_t new_bits = child_intent[j_block_idx] & (~intent[j_block_idx]);
      if (new_bits != 0) {
        uint64_t mask = (j_bit_idx == 0) ? 0 : ((1ULL << j_bit_idx) - 1);
        if ((new_bits & mask) != 0) { is_canonical = false; }
      }
    }

    if (is_canonical) {
      inclose_core_reorder(j, n_objects, n_attributes, N_BLOCKS_M, N_BLOCKS_N,
                           child_extent, child_intent,
                           attr_cols_flat, obj_rows_transposed,
                           ext_i_out, ext_p_out, int_blocks_out,
                           canonicity_tests);
    }
  }
}


// [[Rcpp::export]]
List InClose_Reorder(NumericMatrix I,
                     StringVector attrs,
                     bool verbose = false) {
  Timer timer;
  timer.step("start_reorder_setup");
  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  const size_t N_BLOCKS_M = (n_attributes + 63) / 64;
  const size_t N_BLOCKS_N = (n_objects + 63) / 64;

  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) {
    attr_support[c].second = c;
    int support = 0;
    for (int r = 0; r < n_objects; ++r) {
      if (I(r, c) == 1.0) support++;
    }
    attr_support[c].first = support;
  }

  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    int original_idx = attr_support[j].second;
    new_to_old_attr[j] = original_idx;
    old_to_new_attr[original_idx] = j;
  }

  AttributeCols_Reorder attr_cols_reordered(n_attributes * N_BLOCKS_N, 0);
  ObjectRows_Reorder obj_rows_reordered_transposed(N_BLOCKS_M * n_objects, 0);

  for (int r = 0; r < n_objects; ++r) {
    for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
      if (I(r, c_orig) == 1.0) {
        int c_new = old_to_new_attr[c_orig];

        int block_idx_n = r / 64; int bit_idx_n = r % 64;
        attr_cols_reordered[c_new * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);

        int block_idx_m = c_new / 64; int bit_idx_m = c_new % 64;
        obj_rows_reordered_transposed[block_idx_m * n_objects + r] |= (1ULL << bit_idx_m);
      }
    }
  }

  double canonicity_tests = 0;
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;

  size_t estimated_concepts = (n_objects * n_attributes) / 4;
  if (estimated_concepts < 1000) estimated_concepts = 1000;
  ext_i_out.reserve(estimated_concepts * n_objects / 4);
  ext_p_out.reserve(estimated_concepts + 1);
  int_blocks_out.reserve(estimated_concepts * N_BLOCKS_M);

  std::vector<uint64_t> m_prime(N_BLOCKS_N);
  std::fill(m_prime.begin(), m_prime.end(), 0xFFFFFFFFFFFFFFFF);

  for (int j = 0; j < n_attributes; j++) {
    const uint64_t* col_j_ptr = &attr_cols_reordered[j * N_BLOCKS_N];
    for(size_t k = 0; k < N_BLOCKS_N; ++k) {
      m_prime[k] &= col_j_ptr[k];
    }
  }

  bool m_prime_is_empty = true;
  for(size_t k = 0; k < N_BLOCKS_N; ++k) {
    if (m_prime[k] != 0) {
      m_prime_is_empty = false;
      break;
    }
  }

  if (m_prime_is_empty) {
    ext_p_out.push_back(ext_i_out.size());
    std::vector<uint64_t> intent_M(N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
    int_blocks_out.insert(int_blocks_out.end(), intent_M.begin(), intent_M.end());
  }

  Extent_Reorder initial_extent;
  initial_extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);

  IntentAccumulator_Reorder initial_intent(N_BLOCKS_M);

  for(size_t k = 0; k < N_BLOCKS_M; ++k) {
    const uint64_t* row_k_ptr = &obj_rows_reordered_transposed[k * n_objects];
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    for (int obj_idx : initial_extent) {
      intent_k &= row_k_ptr[obj_idx];
    }
    initial_intent[k] = intent_k;
  }

  timer.step("start_reorder_recursion");

  inclose_core_reorder(-1, n_objects, n_attributes, N_BLOCKS_M, N_BLOCKS_N,
                       initial_extent, initial_intent,
                       attr_cols_reordered, obj_rows_reordered_transposed,
                       ext_i_out, ext_p_out, int_blocks_out,
                       &canonicity_tests);

  timer.step("end_reorder_recursion");

  // 7. EMPAQUETADO SEGURO (MANUAL INITIALIZATION)
  // -----------------------------------------------------------------------
  // NOTA: No usamos initVector/initArray de las librerías externas
  // para evitar conflictos con R_alloc. Inicializamos a mano a NULL.

  SparseVector extents_out;
  // Inicialización manual para garantizar que realloc funcione (actúa como malloc)
  extents_out.i.array = NULL; extents_out.i.used = 0; extents_out.i.size = 0;
  extents_out.p.array = NULL; extents_out.p.used = 0; extents_out.p.size = 0;
  extents_out.x.array = NULL; extents_out.x.used = 0; extents_out.x.size = 0;
  extents_out.length = n_objects;

  size_t total_nnz = ext_i_out.size();
  ensureArray_int_v10(&(extents_out.i), total_nnz);
  if (total_nnz > 0) {
    memcpy(extents_out.i.array, ext_i_out.data(), total_nnz * sizeof(int));
  }
  extents_out.i.used = total_nnz;

  ext_p_out.push_back(total_nnz);
  size_t total_p = ext_p_out.size();
  ensureArray_int_v10(&(extents_out.p), total_p);
  if (total_p > 0) {
    memcpy(extents_out.p.array, ext_p_out.data(), total_p * sizeof(int));
  }
  extents_out.p.used = total_p;

  // Para matriz binaria (ngCMatrix), 'x' no es estrictamente necesario,
  // pero si SparseToS4_fast lo espera (dgCMatrix), lo llenamos de 1s.
  ensureArray_double_v9(&(extents_out.x), total_nnz);
  if (total_nnz > 0) {
    std::fill_n(extents_out.x.array, total_nnz, 1.0);
  }
  extents_out.x.used = total_nnz;

  // Empaquetado de Intents (También manual)
  DoubleArray intents_out;
  intents_out.array = NULL; intents_out.used = 0; intents_out.size = 0;

  size_t total_concepts = ext_p_out.size() - 1;
  size_t total_int_doubles = total_concepts * n_attributes;
  ensureArray_double_v9(&intents_out, total_int_doubles);
  double* intent_out_ptr = intents_out.array;

  std::vector<double> temp_intent(n_attributes);

  for(size_t c = 0; c < total_concepts; ++c) {
    const uint64_t* block_ptr = &int_blocks_out[c * N_BLOCKS_M];
    std::fill(temp_intent.begin(), temp_intent.end(), 0.0);

    for (int j_new = 0; j_new < n_attributes; ++j_new) {
      if (test_bit_native_M_Reorder_flat(block_ptr, j_new)) {
        int j_orig = new_to_old_attr[j_new];
        temp_intent[j_orig] = 1.0;
      }
    }

    if (intent_out_ptr != NULL) {
      memcpy(intent_out_ptr, temp_intent.data(), n_attributes * sizeof(double));
      intent_out_ptr += n_attributes;
    }
  }
  intents_out.used = total_int_doubles;

  S4 intents_S4 = DenseArrayToS4(intents_out, n_attributes);
  S4 extents_S4 = SparseToS4_fast(extents_out);

  // Limpieza Manual (usando free de C, ya que usamos realloc)
  if (extents_out.i.array) free(extents_out.i.array);
  if (extents_out.p.array) free(extents_out.p.array);
  if (extents_out.x.array) free(extents_out.x.array);
  if (intents_out.array) free(intents_out.array);

  timer.step("end_reorder_packaging");

  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = total_concepts,
    _["tests"] = canonicity_tests,
    _["att_intents"] = 0,
    _["timer"] = timer);
  return res;
}
