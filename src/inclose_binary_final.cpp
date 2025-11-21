#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <utility>

using namespace Rcpp;

// =============================================================================
// --- TIPOS DE DATOS ---
// =============================================================================

using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

// =============================================================================
// --- HELPERS DE BITS (INLINE) ---
// =============================================================================

inline bool test_bit_native_M_Reorder(const IntentAccumulator_Reorder& blocks, size_t k) {
  size_t block_idx = k / 64;
  size_t bit_idx = k % 64;
  if (block_idx >= blocks.size()) return false;
  return (blocks[block_idx] & (1ULL << bit_idx)) != 0;
}

inline bool test_bit_native_M_Reorder_flat(const uint64_t* blocks, size_t k) {
  size_t block_idx = k / 64;
  size_t bit_idx = k % 64;
  return (blocks[block_idx] & (1ULL << bit_idx)) != 0;
}

inline bool test_bit_native_N_Reorder(const AttributeCols_Reorder& blocks,
                                      size_t attr_j,
                                      int obj_idx,
                                      size_t N_BLOCKS_N) {
  size_t idx = static_cast<size_t>(obj_idx);
  size_t block_idx = idx / 64;
  size_t bit_idx = idx % 64;
  return (blocks[attr_j * N_BLOCKS_N + block_idx] & (1ULL << bit_idx)) != 0;
}

// =============================================================================
// --- LÓGICA RECURSIVA (CORE) ---
// =============================================================================

void inclose_core_reorder(int y,
                          int n_objects,
                          int n_attributes,
                          const size_t N_BLOCKS_M,
                          const size_t N_BLOCKS_N,
                          const Extent_Reorder& extent,
                          IntentAccumulator_Reorder& intent,
                          const AttributeCols_Reorder& attr_cols_flat,
                          const ObjectRows_Reorder& obj_rows_transposed,
                          std::vector<int>& ext_i_out,
                          std::vector<int>& ext_p_out,
                          std::vector<uint64_t>& int_blocks_out,
                          double& canonicity_tests,
                          int recursion_depth) {

  if (recursion_depth % 500 == 0) Rcpp::checkUserInterrupt();

  ext_p_out.push_back(ext_i_out.size());
  ext_i_out.insert(ext_i_out.end(), extent.begin(), extent.end());
  int_blocks_out.insert(int_blocks_out.end(), intent.begin(), intent.end());

  Extent_Reorder child_extent;
  child_extent.reserve(extent.size());

  IntentAccumulator_Reorder child_intent(N_BLOCKS_M);

  for (int j = y + 1; j < n_attributes; j++) {
    if (test_bit_native_M_Reorder(intent, static_cast<size_t>(j))) continue;

    child_extent.clear();
    for (int obj_idx : extent) {
      if (test_bit_native_N_Reorder(attr_cols_flat, static_cast<size_t>(j), obj_idx, N_BLOCKS_N)) {
        child_extent.push_back(obj_idx);
      }
    }

    if (child_extent.empty()) continue;

    const uint64_t* base_obj_ptr = obj_rows_transposed.data();

    for(size_t k = 0; k < N_BLOCKS_M; ++k) {
      const uint64_t* row_k_ptr = base_obj_ptr + (k * n_objects);
      uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
      for (int obj_idx : child_extent) {
        intent_k &= row_k_ptr[obj_idx];
      }
      child_intent[k] = intent_k;
    }

    canonicity_tests += 1.0;
    bool is_canonical = true;

    size_t j_sz = static_cast<size_t>(j);
    size_t j_block_idx = j_sz / 64;
    size_t j_bit_idx = j_sz % 64;

    for (size_t b = 0; b < j_block_idx; b++) {
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
                           canonicity_tests, recursion_depth + 1);
    }
  }
}

// =============================================================================
// --- FUNCIÓN PRINCIPAL EXPORTADA ---
// =============================================================================

// [[Rcpp::export]]
List InClose_Reorder(RObject I, // Aceptamos RObject genérico (Matrix o S4)
                     Nullable<StringVector> attrs = R_NilValue,
                     bool verbose = false) {

  int n_objects = 0;
  int n_attributes = 0;

  // 1. Detectar tipo y calcular soporte
  std::vector<std::pair<int, int>> attr_support;
  bool is_sparse = false;

  // Punteros para acceso disperso (si aplica)
  IntegerVector sp_p, sp_i, sp_dim;

  if (I.isS4()) {
    // Asumimos dgCMatrix o ngCMatrix
    S4 mat(I);
    // Verificación mínima
    if (!mat.hasSlot("i") || !mat.hasSlot("p") || !mat.hasSlot("Dim")) {
      stop("S4 object must be a valid sparse matrix (slots i, p, Dim).");
    }
    sp_i = mat.slot("i");
    sp_p = mat.slot("p");
    sp_dim = mat.slot("Dim");

    n_objects = sp_dim[0];
    n_attributes = sp_dim[1];
    is_sparse = true;

    // Calcular soporte desde estructura dispersa (Muy rápido)
    attr_support.resize(n_attributes);
    for (int c = 0; c < n_attributes; ++c) {
      attr_support[c].second = c;
      // El soporte es simplemente la diferencia entre punteros de columna
      attr_support[c].first = sp_p[c+1] - sp_p[c];
    }

  } else if (is<NumericMatrix>(I) || is<IntegerMatrix>(I) || is<LogicalMatrix>(I)) {
    // Caso Matriz Densa (Legacy support)
    // Convertimos a IntegerMatrix temporalmente para manejar lógicos/enteros/doubles uniformemente
    IntegerMatrix mat = as<IntegerMatrix>(I);
    n_objects = mat.nrow();
    n_attributes = mat.ncol();

    attr_support.resize(n_attributes);
    for (int c = 0; c < n_attributes; ++c) {
      attr_support[c].second = c;
      int support = 0;
      for (int r = 0; r < n_objects; ++r) {
        if (mat(r, c) != 0) support++;
      }
      attr_support[c].first = support;
    }
  } else {
    stop("Input I must be a matrix or a sparse matrix (dgCMatrix/ngCMatrix).");
  }

  if (n_objects == 0 || n_attributes == 0) return List::create();

  const size_t N_BLOCKS_M = (static_cast<size_t>(n_attributes) + 63) / 64;
  const size_t N_BLOCKS_N = (static_cast<size_t>(n_objects) + 63) / 64;

  // 2. Ordenar Atributos
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    int original_idx = attr_support[j].second;
    new_to_old_attr[j] = original_idx;
    old_to_new_attr[original_idx] = j;
  }

  // 3. Construir Estructuras de Bits (Carga de datos)
  AttributeCols_Reorder attr_cols_reordered(n_attributes * N_BLOCKS_N, 0);
  ObjectRows_Reorder obj_rows_reordered_transposed(N_BLOCKS_M * n_objects, 0);

  if (is_sparse) {
    // --- CARGA OPTIMIZADA PARA SPARSE ---
    for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
      int start_idx = sp_p[c_orig];
      int end_idx = sp_p[c_orig + 1];

      if (start_idx == end_idx) continue; // Columna vacía

      int c_new = old_to_new_attr[c_orig];
      size_t c_new_sz = static_cast<size_t>(c_new);
      size_t block_idx_m = c_new_sz / 64;
      size_t bit_idx_m = c_new_sz % 64;

      for (int k = start_idx; k < end_idx; ++k) {
        int r = sp_i[k]; // Índice de fila

        size_t r_sz = static_cast<size_t>(r);
        size_t block_idx_n = r_sz / 64;
        size_t bit_idx_n = r_sz % 64;

        attr_cols_reordered[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);
        obj_rows_reordered_transposed[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
      }
    }
  } else {
    // --- CARGA PARA DENSA (Fallback) ---
    IntegerMatrix mat = as<IntegerMatrix>(I);
    for (int r = 0; r < n_objects; ++r) {
      for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
        if (mat(r, c_orig) != 0) {
          int c_new = old_to_new_attr[c_orig];
          size_t r_sz = static_cast<size_t>(r);
          size_t c_new_sz = static_cast<size_t>(c_new);

          size_t block_idx_n = r_sz / 64;
          size_t bit_idx_n = r_sz % 64;
          attr_cols_reordered[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);

          size_t block_idx_m = c_new_sz / 64;
          size_t bit_idx_m = c_new_sz % 64;
          obj_rows_reordered_transposed[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
        }
      }
    }
  }

  // 4. Preparar algoritmo
  double canonicity_tests = 0;
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;

  size_t estimated_concepts = (size_t)(n_objects * n_attributes) / 10;
  if (estimated_concepts < 1000) estimated_concepts = 1000;

  try {
    ext_i_out.reserve(estimated_concepts * n_objects / 5);
    ext_p_out.reserve(estimated_concepts + 1);
    int_blocks_out.reserve(estimated_concepts * N_BLOCKS_M);
  } catch(...) {}

  // Bottom Check
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

  // Top Check & Init
  Extent_Reorder initial_extent;
  initial_extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);

  IntentAccumulator_Reorder initial_intent(N_BLOCKS_M);
  const uint64_t* base_obj_ptr = obj_rows_reordered_transposed.data();
  for(size_t k = 0; k < N_BLOCKS_M; ++k) {
    const uint64_t* row_k_ptr = base_obj_ptr + (k * n_objects);
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    for (int obj_idx : initial_extent) {
      intent_k &= row_k_ptr[obj_idx];
    }
    initial_intent[k] = intent_k;
  }

  // Ejecutar Recursión
  inclose_core_reorder(-1, n_objects, n_attributes, N_BLOCKS_M, N_BLOCKS_N,
                       initial_extent, initial_intent,
                       attr_cols_reordered, obj_rows_reordered_transposed,
                       ext_i_out, ext_p_out, int_blocks_out,
                       canonicity_tests, 0);

  // 5. Empaquetado (sin cambios, solo safe wraps)
  ext_p_out.push_back(ext_i_out.size());
  int n_concepts = ext_p_out.size() - 1;

  IntegerVector ext_i_rcpp = wrap(ext_i_out);
  IntegerVector ext_p_rcpp = wrap(ext_p_out);
  NumericVector ext_x_rcpp(ext_i_out.size(), 1.0);

  S4 extents_S4("dgCMatrix");
  extents_S4.slot("i") = ext_i_rcpp;
  extents_S4.slot("p") = ext_p_rcpp;
  extents_S4.slot("x") = ext_x_rcpp;
  extents_S4.slot("Dim") = IntegerVector::create(n_objects, n_concepts);

  std::vector<int> int_i_vec;
  std::vector<int> int_p_vec;
  int_p_vec.reserve(n_concepts + 1);
  int_p_vec.push_back(0);

  if (n_concepts > 0 && n_attributes > 0) {
    size_t estimated_ints = (size_t)n_concepts * (size_t)n_attributes / 5;
    if (estimated_ints > 0) int_i_vec.reserve(estimated_ints);
  }

  for(int c = 0; c < n_concepts; ++c) {
    const uint64_t* block_ptr = &int_blocks_out[c * N_BLOCKS_M];
    int count_c = 0;
    for (int j_new = 0; j_new < n_attributes; ++j_new) {
      if (test_bit_native_M_Reorder_flat(block_ptr, static_cast<size_t>(j_new))) {
        int_i_vec.push_back(new_to_old_attr[j_new]);
        count_c++;
      }
    }
    std::sort(int_i_vec.end() - count_c, int_i_vec.end());
    int_p_vec.push_back(int_p_vec.back() + count_c);
  }

  IntegerVector int_i_rcpp = wrap(int_i_vec);
  IntegerVector int_p_rcpp = wrap(int_p_vec);
  NumericVector int_x_rcpp(int_i_vec.size(), 1.0);

  S4 intents_S4("dgCMatrix");
  intents_S4.slot("i") = int_i_rcpp;
  intents_S4.slot("p") = int_p_rcpp;
  intents_S4.slot("x") = int_x_rcpp;
  intents_S4.slot("Dim") = IntegerVector::create(n_attributes, n_concepts);

  return List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = n_concepts,
    _["tests"] = canonicity_tests,
    _["att_intents"] = 0
  );
}
