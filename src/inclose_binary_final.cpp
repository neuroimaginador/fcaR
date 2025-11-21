#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <utility>

using namespace Rcpp;

// =============================================================================
// --- TIPOS Y MACROS SEGUROS ---
// =============================================================================

// Usamos tipos explícitos de 64 bits para evitar problemas de alineación
using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

// =============================================================================
// --- HELPERS (SIN PUNTEROS CRUDOS SIEMPRE QUE SEA POSIBLE) ---
// =============================================================================

// Helper seguro para verificar bits en el acumulador (Intent)
inline bool check_bit_M(const IntentAccumulator_Reorder& blocks, size_t k) {
  size_t block_idx = k >> 6; // k / 64
  size_t bit_idx = k & 63;   // k % 64
  return (blocks[block_idx] & (1ULL << bit_idx)) != 0;
}

// Helper seguro para verificar bits en la matriz plana de columnas
inline bool check_bit_N(const AttributeCols_Reorder& blocks,
                        size_t attr_j,
                        int obj_idx,
                        size_t N_BLOCKS_N) {
  size_t idx = static_cast<size_t>(obj_idx);
  size_t block_idx = idx >> 6;
  size_t bit_idx = idx & 63;
  // Acceso plano: Columna * Stride + Offset
  return (blocks[attr_j * N_BLOCKS_N + block_idx] & (1ULL << bit_idx)) != 0;
}

// Helper para verificar bits en un bloque específico (usado en output)
inline bool check_bit_flat_block(const uint64_t* block_ptr, size_t k) {
  size_t block_idx = k >> 6;
  size_t bit_idx = k & 63;
  return (block_ptr[block_idx] & (1ULL << bit_idx)) != 0;
}

// =============================================================================
// --- CORE RECURSIVO ---
// =============================================================================

void inclose_core_reorder(int y,
                          int n_objects,
                          int n_attributes,
                          const size_t N_BLOCKS_M,
                          const size_t N_BLOCKS_N,
                          const Extent_Reorder& extent,
                          IntentAccumulator_Reorder& intent,
                          const AttributeCols_Reorder& attr_cols_flat,
                          const ObjectRows_Reorder& obj_rows_flat, // Pasamos el vector, no puntero
                          std::vector<int>& ext_i_out,
                          std::vector<int>& ext_p_out,
                          std::vector<uint64_t>& int_blocks_out,
                          double& canonicity_tests,
                          int recursion_depth) {

  // Check de interrupción menos frecuente para no saturar
  if (recursion_depth % 1000 == 0) Rcpp::checkUserInterrupt();

  // Guardar concepto actual
  ext_p_out.push_back(ext_i_out.size());
  ext_i_out.insert(ext_i_out.end(), extent.begin(), extent.end());
  int_blocks_out.insert(int_blocks_out.end(), intent.begin(), intent.end());

  Extent_Reorder child_extent;
  child_extent.reserve(extent.size());

  IntentAccumulator_Reorder child_intent(N_BLOCKS_M);

  for (int j = y + 1; j < n_attributes; j++) {
    // Si el atributo j ya está en el intent, saltar
    if (check_bit_M(intent, static_cast<size_t>(j))) continue;

    // Construir nuevo extent (Intersección)
    child_extent.clear();
    for (int obj_idx : extent) {
      if (check_bit_N(attr_cols_flat, static_cast<size_t>(j), obj_idx, N_BLOCKS_N)) {
        child_extent.push_back(obj_idx);
      }
    }

    if (child_extent.empty()) continue;

    // Construir nuevo intent (Intersección de filas de objetos)
    // AQUÍ ELIMINAMOS LA ARITMÉTICA DE PUNTEROS PELIGROSA
    // En vez de `ptr + k*n`, calculamos índices.

    for(size_t k = 0; k < N_BLOCKS_M; ++k) {
      uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
      size_t block_offset = k * n_objects; // Inicio del bloque k en la matriz transpuesta

      for (int obj_idx : child_extent) {
        // obj_rows_flat está organizada como [Bloque0_Obj0, Bloque0_Obj1... | Bloque1...]
        intent_k &= obj_rows_flat[block_offset + obj_idx];
      }
      child_intent[k] = intent_k;
    }

    canonicity_tests += 1.0;
    bool is_canonical = true;

    size_t j_sz = static_cast<size_t>(j);
    size_t j_block_idx = j_sz >> 6;
    size_t j_bit_idx = j_sz & 63;

    // Test de canonicidad
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
                           attr_cols_flat, obj_rows_flat,
                           ext_i_out, ext_p_out, int_blocks_out,
                           canonicity_tests, recursion_depth + 1);
    }
  }
}

// =============================================================================
// --- MAIN EXPORT ---
// =============================================================================

// [[Rcpp::export]]
List InClose_Reorder(RObject I,
                     Nullable<StringVector> attrs = R_NilValue,
                     bool verbose = false) {

  int n_objects = 0;
  int n_attributes = 0;

  // Vectores temporales para índices de matriz dispersa
  // Usamos std::vector para desacoplar de la memoria de R si es necesario
  std::vector<int> sp_i_vec;
  std::vector<int> sp_p_vec;
  bool is_sparse = false;

  // --- 1. DETECCIÓN Y VALIDACIÓN DE ENTRADA ---

  if (I.isS4()) {
    S4 mat(I);
    // Verificar clases válidas
    bool valid_class = false;
    if (mat.hasSlot("class")) { // A veces no es directo checkear class en S4 genérico
      // Asumimos válido si tiene los slots
    }
    if (!mat.hasSlot("i") || !mat.hasSlot("p") || !mat.hasSlot("Dim")) {
      stop("Input S4 object must have slots 'i', 'p', and 'Dim' (e.g. dgCMatrix).");
    }

    IntegerVector dim = mat.slot("Dim");
    n_objects = dim[0];
    n_attributes = dim[1];

    IntegerVector i_R = mat.slot("i");
    IntegerVector p_R = mat.slot("p");

    // Copiar a std::vector para acceso seguro y rápido (evita overhead de Rcpp wrapper en loops)
    sp_i_vec.assign(i_R.begin(), i_R.end());
    sp_p_vec.assign(p_R.begin(), p_R.end());
    is_sparse = true;

    // VALIDACIÓN DE SEGURIDAD (CRÍTICO PARA EVITAR SEGFAULTS)
    if (sp_p_vec.size() != (size_t)(n_attributes + 1)) stop("Invalid sparse matrix: p slot length mismatch.");
    // Verificar bounds de i
    for(int row_idx : sp_i_vec) {
      if(row_idx < 0 || row_idx >= n_objects) stop("Invalid sparse matrix: row index out of bounds.");
    }

  } else {
    // Conversión manual segura para matriz densa
    IntegerMatrix mat;
    try {
      mat = as<IntegerMatrix>(I);
    } catch (...) {
      stop("Input must be a matrix or a sparse matrix.");
    }
    n_objects = mat.nrow();
    n_attributes = mat.ncol();

    // Convertir a formato disperso interno (CSR/CSC) para unificar lógica
    sp_p_vec.resize(n_attributes + 1, 0);
    for (int c = 0; c < n_attributes; ++c) {
      int count = 0;
      for (int r = 0; r < n_objects; ++r) {
        if (mat(r, c) != 0) {
          sp_i_vec.push_back(r);
          count++;
        }
      }
      sp_p_vec[c+1] = sp_p_vec[c] + count;
    }
    is_sparse = true; // Ahora tratamos todo como sparse internamente
  }

  if (n_objects == 0 || n_attributes == 0) return List::create();

  // --- 2. CÁLCULO DE SOPORTE ---

  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) {
    attr_support[c].second = c;
    // Soporte = p[c+1] - p[c]
    attr_support[c].first = sp_p_vec[c+1] - sp_p_vec[c];
  }
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    int original_idx = attr_support[j].second;
    new_to_old_attr[j] = original_idx;
    old_to_new_attr[original_idx] = j;
  }

  // --- 3. POBLAR BITSETS (CON CHECK DE BOUNDS) ---

  const size_t N_BLOCKS_M = (static_cast<size_t>(n_attributes) + 63) / 64;
  const size_t N_BLOCKS_N = (static_cast<size_t>(n_objects) + 63) / 64;

  AttributeCols_Reorder attr_cols(n_attributes * N_BLOCKS_N, 0);
  ObjectRows_Reorder obj_rows(N_BLOCKS_M * n_objects, 0);

  for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
    int start = sp_p_vec[c_orig];
    int end = sp_p_vec[c_orig + 1];

    if (start == end) continue;

    int c_new = old_to_new_attr[c_orig];
    size_t c_new_sz = static_cast<size_t>(c_new);

    // Pre-cálculo índices columna
    size_t block_idx_m = c_new_sz >> 6;
    size_t bit_idx_m = c_new_sz & 63;

    for (int k = start; k < end; ++k) {
      int r = sp_i_vec[k];
      size_t r_sz = static_cast<size_t>(r);

      // Escribir en AttrCols (Indexado por columna nueva)
      size_t block_idx_n = r_sz >> 6;
      size_t bit_idx_n = r_sz & 63;

      attr_cols[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);

      // Escribir en ObjRows (Transpuesta)
      // Layout: [BlockM 0][Rows...] [BlockM 1][Rows...]
      obj_rows[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
    }
  }

  // --- 4. PREPARAR RECURSIÓN ---

  double canonicity_tests = 0;
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;

  size_t est_concepts = (size_t)(n_objects * n_attributes) / 10;
  if (est_concepts < 1000) est_concepts = 1000;
  try {
    ext_i_out.reserve(est_concepts * n_objects / 5);
    ext_p_out.reserve(est_concepts + 1);
    int_blocks_out.reserve(est_concepts * N_BLOCKS_M);
  } catch(...) {}

  // Bottom Check (Intersección global)
  std::vector<uint64_t> m_prime(N_BLOCKS_N);
  std::fill(m_prime.begin(), m_prime.end(), 0xFFFFFFFFFFFFFFFF);

  for (int j = 0; j < n_attributes; j++) {
    // Iteramos manual en vez de puntero para seguridad
    size_t offset = j * N_BLOCKS_N;
    for(size_t k = 0; k < N_BLOCKS_N; ++k) {
      m_prime[k] &= attr_cols[offset + k];
    }
  }

  bool m_prime_is_empty = true;
  for(size_t k = 0; k < N_BLOCKS_N; ++k) {
    if (m_prime[k] != 0) { m_prime_is_empty = false; break; }
  }

  if (m_prime_is_empty) {
    ext_p_out.push_back(ext_i_out.size());
    // Intent vacío (lleno de 1s lógica inversa o 0s lógica directa? InClose suele usar 1s para bottom)
    // Asumimos lógica estándar: Bottom concept tiene todos los atributos? No, extent vacío -> todos atributos.
    std::vector<uint64_t> intent_M(N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
    int_blocks_out.insert(int_blocks_out.end(), intent_M.begin(), intent_M.end());
  }

  // Top Check
  Extent_Reorder initial_extent;
  initial_extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);

  IntentAccumulator_Reorder initial_intent(N_BLOCKS_M);

  // Calcular intent inicial
  for(size_t k = 0; k < N_BLOCKS_M; ++k) {
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    size_t block_offset = k * n_objects;
    for (int obj_idx : initial_extent) {
      intent_k &= obj_rows[block_offset + obj_idx];
    }
    initial_intent[k] = intent_k;
  }

  // Lanzar InClose
  inclose_core_reorder(-1, n_objects, n_attributes, N_BLOCKS_M, N_BLOCKS_N,
                       initial_extent, initial_intent,
                       attr_cols, obj_rows,
                       ext_i_out, ext_p_out, int_blocks_out,
                       canonicity_tests, 0);

  // --- 5. EMPAQUETADO ---

  ext_p_out.push_back(ext_i_out.size());
  int n_concepts = ext_p_out.size() - 1;

  IntegerVector ext_i_rcpp = wrap(ext_i_out);
  IntegerVector ext_p_rcpp = wrap(ext_p_out);
  NumericVector ext_x_rcpp(ext_i_out.size(), 1.0); // Todo 1

  S4 extents_S4("dgCMatrix");
  extents_S4.slot("i") = ext_i_rcpp;
  extents_S4.slot("p") = ext_p_rcpp;
  extents_S4.slot("x") = ext_x_rcpp;
  extents_S4.slot("Dim") = IntegerVector::create(n_objects, n_concepts);

  std::vector<int> int_i_vec;
  std::vector<int> int_p_vec;
  int_p_vec.reserve(n_concepts + 1);
  int_p_vec.push_back(0);

  if (n_concepts > 0) int_i_vec.reserve(n_concepts * n_attributes / 5);

  for(int c = 0; c < n_concepts; ++c) {
    const uint64_t* block_ptr = &int_blocks_out[c * N_BLOCKS_M]; // Aquí es seguro, vector completo
    int count_c = 0;
    for (int j_new = 0; j_new < n_attributes; ++j_new) {
      if (check_bit_flat_block(block_ptr, static_cast<size_t>(j_new))) {
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
    _["tests"] = canonicity_tests
  );
}
