#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include <vector>
#include <algorithm>
#include <utility>
#include <cstring>

using namespace Rcpp;

// =============================================================================
// --- TIPOS Y HELPERS PARA REORDER ---
// =============================================================================

using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

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

  // 1. Reordenación
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

  // Bottom check
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
    if (m_prime[k] != 0) { m_prime_is_empty = false; break; }
  }

  if (m_prime_is_empty) {
    ext_p_out.push_back(ext_i_out.size());
    std::vector<uint64_t> intent_M(N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
    int_blocks_out.insert(int_blocks_out.end(), intent_M.begin(), intent_M.end());
  }

  // Top check
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

  // ---------------------------------------------------------------------------
  // FASE DE EMPAQUETADO "MODERNA" (SIN MALLOC/REALLOC NI STRUCTS C)
  // Convertimos std::vector -> Objetos S4 R directamente
  // ---------------------------------------------------------------------------

  // 1. Construir EXTENTS (dgCMatrix: Rows=Objetos, Cols=Conceptos)
  ext_p_out.push_back(ext_i_out.size()); // Final p
  int n_concepts = ext_p_out.size() - 1;

  // Crear vectores Rcpp (esto copia memoria de forma segura al heap de R)
  IntegerVector ext_i_rcpp = wrap(ext_i_out);
  IntegerVector ext_p_rcpp = wrap(ext_p_out);
  NumericVector ext_x_rcpp(ext_i_out.size(), 1.0); // Todo 1s para binario

  // Crear el objeto S4 directamente
  S4 extents_S4("dgCMatrix");
  extents_S4.slot("i") = ext_i_rcpp;
  extents_S4.slot("p") = ext_p_rcpp;
  extents_S4.slot("x") = ext_x_rcpp;
  extents_S4.slot("Dim") = IntegerVector::create(n_objects, n_concepts);

  // 2. Construir INTENTS (dgCMatrix: Rows=Atributos, Cols=Conceptos)
  // Necesitamos reconstruir la matriz dispersa de intents a partir de los bloques de bits
  std::vector<int> int_i_vec;
  std::vector<int> int_p_vec;
  int_p_vec.reserve(n_concepts + 1);
  int_p_vec.push_back(0);

  // Estimación de reserva para int_i (densidad 20% por defecto)
  int_i_vec.reserve(n_concepts * n_attributes * 0.2);

  for(int c = 0; c < n_concepts; ++c) {
    const uint64_t* block_ptr = &int_blocks_out[c * N_BLOCKS_M];
    int count_c = 0;

    for (int j_new = 0; j_new < n_attributes; ++j_new) {
      // Verificar bit en estructura reordenada
      if (test_bit_native_M_Reorder_flat(block_ptr, j_new)) {
        // Recuperar índice original del atributo
        int j_orig = new_to_old_attr[j_new];
        int_i_vec.push_back(j_orig);
        count_c++;
      }
    }
    // IMPORTANTE: dgCMatrix requiere índices de fila ordenados dentro de cada columna.
    // Como 'new_to_old_attr' desordena, debemos ordenar este bloque recién insertado.
    std::sort(int_i_vec.end() - count_c, int_i_vec.end());

    int_p_vec.push_back(int_p_vec.back() + count_c);
  }

  // Convertir a Rcpp
  IntegerVector int_i_rcpp = wrap(int_i_vec);
  IntegerVector int_p_rcpp = wrap(int_p_vec);
  NumericVector int_x_rcpp(int_i_vec.size(), 1.0);

  S4 intents_S4("dgCMatrix");
  intents_S4.slot("i") = int_i_rcpp;
  intents_S4.slot("p") = int_p_rcpp;
  intents_S4.slot("x") = int_x_rcpp;
  intents_S4.slot("Dim") = IntegerVector::create(n_attributes, n_concepts);

  timer.step("end_reorder_packaging");

  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = n_concepts,
    _["tests"] = canonicity_tests,
    _["att_intents"] = 0,
    _["timer"] = timer);

  return res;
}
