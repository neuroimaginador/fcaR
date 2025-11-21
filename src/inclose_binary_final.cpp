#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h> // Timer recuperado
#include <vector>
#include <algorithm>
#include <Rinternals.h> // API C pura de R (Velocidad máxima)

using namespace Rcpp;

// =============================================================================
// --- TIPOS OPTIMIZADOS ---
// =============================================================================
using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

// =============================================================================
// --- HELPER GLOBAL (Inline para inyección directa de código) ---
// =============================================================================
inline bool check_bit_flat_block(const uint64_t* block_ptr, size_t k) {
  // Operación a nivel de bit pura, sin saltos condicionales
  return (block_ptr[k >> 6] & (1ULL << (k & 63))) != 0;
}

// =============================================================================
// --- CLASE SOLVER (Gestión de Memoria en Heap) ---
// =============================================================================
class InCloseSolver {
public:
  int n_objects;
  int n_attributes;
  size_t N_BLOCKS_M;
  size_t N_BLOCKS_N;

  // Datos (Bitsets) - Acceso directo
  AttributeCols_Reorder attr_cols;
  ObjectRows_Reorder obj_rows;

  // Resultados
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;
  double canonicity_tests;

  InCloseSolver(int n_obj, int n_att) :
    n_objects(n_obj), n_attributes(n_att), canonicity_tests(0.0) {

    N_BLOCKS_M = (static_cast<size_t>(n_attributes) + 63) / 64;
    N_BLOCKS_N = (static_cast<size_t>(n_objects) + 63) / 64;

    // Reserva única de memoria (evita fragmentación)
    attr_cols.resize(n_attributes * N_BLOCKS_N, 0);
    obj_rows.resize(N_BLOCKS_M * n_objects, 0);

    // Heurística de reserva de salida (Ajustada para alto rendimiento)
    // Asumimos un factor de densidad razonable para evitar reallocs
    size_t est = (size_t)(n_objects * n_attributes) / 5;
    if (est < 2000) est = 2000;

    ext_i_out.reserve(est * n_objects / 4);
    ext_p_out.reserve(est + 1);
    int_blocks_out.reserve(est * N_BLOCKS_M);
  }

  // Helpers internos (Inline forzado)
  inline bool check_bit_M(const IntentAccumulator_Reorder& blocks, size_t k) {
    size_t block_idx = k >> 6;
    if (block_idx >= blocks.size()) return false;
    return (blocks[block_idx] & (1ULL << (k & 63))) != 0;
  }

  inline bool check_bit_N(size_t attr_j, int obj_idx) {
    size_t idx = static_cast<size_t>(obj_idx);
    return (attr_cols[attr_j * N_BLOCKS_N + (idx >> 6)] & (1ULL << (idx & 63))) != 0;
  }

  // Núcleo Recursivo (Hot Path)
  void solve_recursive(int y,
                       const Extent_Reorder& extent,
                       IntentAccumulator_Reorder& intent,
                       int recursion_depth) {

    // Check de interrupción ligero (bitwise mask es más rápido que módulo)
    if ((recursion_depth & 1023) == 0) Rcpp::checkUserInterrupt();

    // 1. Guardar concepto
    ext_p_out.push_back(ext_i_out.size());
    ext_i_out.insert(ext_i_out.end(), extent.begin(), extent.end());
    int_blocks_out.insert(int_blocks_out.end(), intent.begin(), intent.end());

    Extent_Reorder child_extent;
    child_extent.reserve(extent.size());
    IntentAccumulator_Reorder child_intent(N_BLOCKS_M);

    for (int j = y + 1; j < n_attributes; j++) {
      // Poda rápida
      if (check_bit_M(intent, static_cast<size_t>(j))) continue;

      child_extent.clear();
      for (int obj_idx : extent) {
        if (check_bit_N(static_cast<size_t>(j), obj_idx)) {
          child_extent.push_back(obj_idx);
        }
      }

      if (child_extent.empty()) continue;

      // Cálculo del Intent (Intersección Vectorizada implícita por CPU de 64bits)
      for(size_t k = 0; k < N_BLOCKS_M; ++k) {
        uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
        size_t block_offset = k * n_objects;
        for (int obj_idx : child_extent) {
          intent_k &= obj_rows[block_offset + obj_idx];
        }
        child_intent[k] = intent_k;
      }

      canonicity_tests += 1.0;
      bool is_canonical = true;
      size_t j_sz = static_cast<size_t>(j);
      size_t j_block_idx = j_sz >> 6;
      size_t j_bit_idx = j_sz & 63;

      // Test de canonicidad (Optimizado para fallar rápido)
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
        solve_recursive(j, child_extent, child_intent, recursion_depth + 1);
      }
    }
  }
};

// =============================================================================
// --- FUNCIÓN PRINCIPAL (API C PURA PARA VELOCIDAD) ---
// =============================================================================

// [[Rcpp::export]]
List InClose_Reorder(SEXP sp_i_sexp, SEXP sp_p_sexp, SEXP dim_sexp, bool verbose = false) {

  Timer timer;
  timer.step("setup");

  // Acceso directo a punteros de R (Sin overhead de Rcpp wrappers)
  int* i_ptr = INTEGER(sp_i_sexp);
  int* p_ptr = INTEGER(sp_p_sexp);
  int* dim_ptr = INTEGER(dim_sexp);

  int n_i = Rf_length(sp_i_sexp);
  int n_p = Rf_length(sp_p_sexp);
  int n_objects = dim_ptr[0];
  int n_attributes = dim_ptr[1];

  // Copia ultrarrápida a memoria C++ (memcpy implícito en constructor vector)
  std::vector<int> sp_i_vec(i_ptr, i_ptr + n_i);
  std::vector<int> sp_p_vec(p_ptr, p_ptr + n_p);

  // Instanciar Solver
  InCloseSolver solver(n_objects, n_attributes);

  // --- FASE 1: SOPORTE Y REORDENACIÓN ---
  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) {
    attr_support[c] = {sp_p_vec[c+1] - sp_p_vec[c], c};
  }
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    new_to_old_attr[j] = attr_support[j].second;
    old_to_new_attr[attr_support[j].second] = j;
  }

  // --- FASE 2: CARGA DE BITSETS ---
  for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
    int start = sp_p_vec[c_orig];
    int end = sp_p_vec[c_orig + 1];
    if (start >= end) continue;

    int c_new = old_to_new_attr[c_orig];
    size_t c_new_sz = static_cast<size_t>(c_new);
    size_t block_idx_m = c_new_sz >> 6;
    size_t bit_idx_m = c_new_sz & 63;

    for (int k = start; k < end; ++k) {
      int r = sp_i_vec[k];
      size_t r_sz = static_cast<size_t>(r);
      size_t block_idx_n = r_sz >> 6;
      size_t bit_idx_n = r_sz & 63;

      solver.attr_cols[c_new_sz * solver.N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);
      solver.obj_rows[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
    }
  }

  // Liberar memoria inmediatamente para dar espacio a la recursión
  sp_i_vec.clear(); sp_i_vec.shrink_to_fit();
  sp_p_vec.clear(); sp_p_vec.shrink_to_fit();

  // --- FASE 3: ALGORITMO ---

  // Bottom Check
  std::vector<uint64_t> m_prime(solver.N_BLOCKS_N, 0xFFFFFFFFFFFFFFFF);
  for (int j = 0; j < n_attributes; j++) {
    size_t offset = j * solver.N_BLOCKS_N;
    for(size_t k = 0; k < solver.N_BLOCKS_N; ++k) m_prime[k] &= solver.attr_cols[offset + k];
  }
  bool m_prime_is_empty = true;
  for(size_t k = 0; k < solver.N_BLOCKS_N; ++k) if (m_prime[k] != 0) { m_prime_is_empty = false; break; }

  if (m_prime_is_empty) {
    solver.ext_p_out.push_back(solver.ext_i_out.size());
    solver.int_blocks_out.insert(solver.int_blocks_out.end(), solver.N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
  }

  // Top Check e Inicialización
  Extent_Reorder initial_extent;
  initial_extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);

  IntentAccumulator_Reorder initial_intent(solver.N_BLOCKS_M);
  for(size_t k = 0; k < solver.N_BLOCKS_M; ++k) {
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    size_t block_offset = k * n_objects;
    for (int obj_idx : initial_extent) intent_k &= solver.obj_rows[block_offset + obj_idx];
    initial_intent[k] = intent_k;
  }

  timer.step("recursion");
  solver.solve_recursive(-1, initial_extent, initial_intent, 0);
  timer.step("end_recursion");

  // --- FASE 4: EMPAQUETADO ---
  solver.ext_p_out.push_back(solver.ext_i_out.size());
  int n_concepts = solver.ext_p_out.size() - 1;

  // Construcción segura de objetos S4 de salida
  S4 extents_S4("dgCMatrix");
  extents_S4.slot("i") = wrap(solver.ext_i_out);
  extents_S4.slot("p") = wrap(solver.ext_p_out);
  extents_S4.slot("x") = NumericVector(solver.ext_i_out.size(), 1.0);
  extents_S4.slot("Dim") = IntegerVector::create(n_objects, n_concepts);

  std::vector<int> int_i_vec;
  std::vector<int> int_p_vec;
  int_p_vec.reserve(n_concepts + 1);
  int_p_vec.push_back(0);

  if (n_concepts > 0) int_i_vec.reserve(n_concepts * n_attributes / 2);

  for(int c = 0; c < n_concepts; ++c) {
    const uint64_t* block_ptr = &solver.int_blocks_out[c * solver.N_BLOCKS_M];
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

  S4 intents_S4("dgCMatrix");
  intents_S4.slot("i") = wrap(int_i_vec);
  intents_S4.slot("p") = wrap(int_p_vec);
  intents_S4.slot("x") = NumericVector(int_i_vec.size(), 1.0);
  intents_S4.slot("Dim") = IntegerVector::create(n_attributes, n_concepts);

  timer.step("packaging");

  return List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = n_concepts,
    _["tests"] = solver.canonicity_tests,
    _["att_intents"] = 0,
    _["timer"] = timer
  );
}
