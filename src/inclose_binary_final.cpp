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

// Estructura para simular la recursión en el Heap (Iterativo)
struct InCloseContext {
  int y; // Último atributo procesado
  Extent_Reorder extent;
  IntentAccumulator_Reorder intent;
};

// =============================================================================
// --- HELPERS (BITWISE OPTIMIZADOS) ---
// =============================================================================

inline bool check_bit_M(const IntentAccumulator_Reorder& blocks, size_t k) {
  size_t block_idx = k >> 6;
  if (block_idx >= blocks.size()) return false;
  return (blocks[block_idx] & (1ULL << (k & 63))) != 0;
}

inline bool check_bit_N(const AttributeCols_Reorder& blocks,
                        size_t attr_j,
                        int obj_idx,
                        size_t N_BLOCKS_N) {
  size_t idx = static_cast<size_t>(obj_idx);
  // Acceso plano optimizado
  return (blocks[attr_j * N_BLOCKS_N + (idx >> 6)] & (1ULL << (idx & 63))) != 0;
}

inline bool check_bit_flat_block(const uint64_t* block_ptr, size_t k) {
  return (block_ptr[k >> 6] & (1ULL << (k & 63))) != 0;
}

// =============================================================================
// --- FUNCIÓN PRINCIPAL (ITERATIVA) ---
// =============================================================================

// [[Rcpp::export]]
List InClose_Reorder(RObject I, bool verbose = false) {

  // --- 1. INICIALIZACIÓN Y VALIDACIÓN ---
  int n_objects = 0;
  int n_attributes = 0;

  std::vector<int> sp_i_vec;
  std::vector<int> sp_p_vec;

  // Detección de tipos robusta
  if (I.isS4()) {
    S4 mat(I);
    if (!mat.hasSlot("i") || !mat.hasSlot("p") || !mat.hasSlot("Dim")) {
      stop("Input S4 object must be a valid dgCMatrix/ngCMatrix.");
    }
    IntegerVector dim = mat.slot("Dim");
    n_objects = dim[0];
    n_attributes = dim[1];
    IntegerVector i_R = mat.slot("i");
    IntegerVector p_R = mat.slot("p");

    sp_i_vec.assign(i_R.begin(), i_R.end());
    sp_p_vec.assign(p_R.begin(), p_R.end());

    // Validación crítica de bounds
    if (sp_p_vec.size() != (size_t)(n_attributes + 1)) stop("Invalid sparse matrix: p slot mismatch.");
    if (n_objects > 0) {
      for(int r : sp_i_vec) {
        if(r < 0 || r >= n_objects) stop("Invalid sparse matrix: row index out of bounds.");
      }
    }
  } else {
    IntegerMatrix mat = as<IntegerMatrix>(I);
    n_objects = mat.nrow();
    n_attributes = mat.ncol();
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
  }

  if (n_objects == 0 || n_attributes == 0) return List::create();

  // --- 2. SOPORTE Y REORDENACIÓN ---

  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) {
    attr_support[c] = {sp_p_vec[c+1] - sp_p_vec[c], c};
  }
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    int original_idx = attr_support[j].second;
    new_to_old_attr[j] = original_idx;
    old_to_new_attr[original_idx] = j;
  }

  // --- 3. POBLAR BITSETS (HEAP SAFE) ---

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
    size_t block_idx_m = c_new_sz >> 6;
    size_t bit_idx_m = c_new_sz & 63;

    for (int k = start; k < end; ++k) {
      int r = sp_i_vec[k];
      size_t r_sz = static_cast<size_t>(r);
      size_t block_idx_n = r_sz >> 6;
      size_t bit_idx_n = r_sz & 63;

      attr_cols[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);
      obj_rows[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
    }
  }

  // --- 4. ALGORITMO ITERATIVO (IN-CLOSE SIN RECURSIÓN) ---

  double canonicity_tests = 0;
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;

  // Estimación de reserva
  size_t est_concepts = (size_t)(n_objects * n_attributes) / 10;
  if (est_concepts < 1000) est_concepts = 1000;
  try {
    ext_i_out.reserve(est_concepts * n_objects / 5);
    ext_p_out.reserve(est_concepts + 1);
    int_blocks_out.reserve(est_concepts * N_BLOCKS_M);
  } catch(...) {}

  // -- Bottom Concept --
  std::vector<uint64_t> m_prime(N_BLOCKS_N, 0xFFFFFFFFFFFFFFFF);
  for (int j = 0; j < n_attributes; j++) {
    size_t offset = j * N_BLOCKS_N;
    for(size_t k = 0; k < N_BLOCKS_N; ++k) m_prime[k] &= attr_cols[offset + k];
  }
  bool m_prime_is_empty = true;
  for(size_t k = 0; k < N_BLOCKS_N; ++k) if (m_prime[k] != 0) { m_prime_is_empty = false; break; }

  if (m_prime_is_empty) {
    ext_p_out.push_back(ext_i_out.size());
    std::vector<uint64_t> intent_M(N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
    int_blocks_out.insert(int_blocks_out.end(), intent_M.begin(), intent_M.end());
  }

  // -- Pila para Iteración (DFS) --
  // Usamos un vector como pila LIFO
  std::vector<InCloseContext> stack;
  stack.reserve(n_attributes); // Profundidad máxima teórica

  // -- Top Concept (Root) --
  InCloseContext root;
  root.y = -1;
  root.extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) root.extent.push_back(i);

  root.intent.resize(N_BLOCKS_M);
  for(size_t k = 0; k < N_BLOCKS_M; ++k) {
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    size_t block_offset = k * n_objects;
    for (int obj_idx : root.extent) intent_k &= obj_rows[block_offset + obj_idx];
    root.intent[k] = intent_k;
  }

  stack.push_back(root);

  int iter_counter = 0;

  // Bucle principal: Reemplaza la recursión
  while (!stack.empty()) {
    if (++iter_counter % 1000 == 0) Rcpp::checkUserInterrupt();

    // Tomamos el contexto actual (copia necesaria para procesar hijos)
    InCloseContext current = stack.back();
    stack.pop_back();

    // 1. Guardar concepto
    ext_p_out.push_back(ext_i_out.size());
    ext_i_out.insert(ext_i_out.end(), current.extent.begin(), current.extent.end());
    int_blocks_out.insert(int_blocks_out.end(), current.intent.begin(), current.intent.end());

    // 2. Generar hijos
    // IMPORTANTE: Para mantener el orden DFS correcto de In-Close, iteramos j normalmente
    // pero debemos empujar al stack en orden inverso o procesar cuidadosamente.
    // In-Close estándar procesa (y, n) en bucle.
    // Para simular la recursión exacta:
    //   for j = y+1 to n:
    //      compute child
    //      if canonical -> InClose(child)
    // Esto significa que el bucle j es "horizontal". La llamada recursiva es "vertical".
    // En iterativo, esto es difícil de hacer con una sola pila sin invertir el bucle.
    // SOLUCIÓN: Hacemos el bucle j aquí y empujamos al stack.
    // PERO cuidado: In-Close es "depth-first". Si empujamos todos los j hijos, procesaremos el último j primero.
    // Para respetar el orden (aunque en retículos el orden de descubrimiento no altera el resultado final, solo el orden en la lista),
    // podemos iterar j de n_attributes-1 bajando hasta y+1.

    for (int j = n_attributes - 1; j > current.y; --j) {

      if (check_bit_M(current.intent, static_cast<size_t>(j))) continue;

      // Calcular Extent Hijo
      Extent_Reorder child_extent;
      child_extent.reserve(current.extent.size());
      for (int obj_idx : current.extent) {
        if (check_bit_N(attr_cols, static_cast<size_t>(j), obj_idx, N_BLOCKS_N)) {
          child_extent.push_back(obj_idx);
        }
      }

      if (child_extent.empty()) continue;

      // Calcular Intent Hijo
      IntentAccumulator_Reorder child_intent(N_BLOCKS_M);
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

      for (size_t b = 0; b < j_block_idx; b++) {
        if ((child_intent[b] & (~current.intent[b])) != 0) { is_canonical = false; break; }
      }
      if (is_canonical) {
        uint64_t new_bits = child_intent[j_block_idx] & (~current.intent[j_block_idx]);
        if (new_bits != 0) {
          uint64_t mask = (j_bit_idx == 0) ? 0 : ((1ULL << j_bit_idx) - 1);
          if ((new_bits & mask) != 0) { is_canonical = false; }
        }
      }

      if (is_canonical) {
        // Empujar al stack para procesar después (DFS)
        InCloseContext child;
        child.y = j;
        child.extent = std::move(child_extent); // Movemos para evitar copia
        child.intent = std::move(child_intent);
        stack.push_back(std::move(child));
      }
    }
  }

  // --- 5. EMPAQUETADO FINAL ---

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
  if (n_concepts > 0) int_i_vec.reserve(n_concepts * n_attributes / 5);

  for(int c = 0; c < n_concepts; ++c) {
    // Nota: Debido al stack LIFO, los conceptos pueden salir en orden distinto al recursivo puro
    // Pero para find_concepts() el orden no es estricto mientras estén todos.
    // Si necesitas orden estricto por soporte, ordénalos después en R.
    const uint64_t* block_ptr = &int_blocks_out[c * N_BLOCKS_M];
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
