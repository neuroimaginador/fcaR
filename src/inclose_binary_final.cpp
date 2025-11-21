#include <Rcpp.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// --- TIPOS ---
using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

// Contexto
struct InCloseContext {
  int n_objects;
  int n_attributes;
  size_t N_BLOCKS_M;
  size_t N_BLOCKS_N;
  const AttributeCols_Reorder& attr_cols;
  const ObjectRows_Reorder& obj_rows;
  std::vector<int>* ext_i_out;
  std::vector<int>* ext_p_out;
  std::vector<uint64_t>* int_blocks_out;
  double* canonicity_tests;

  InCloseContext(int n_obj, int n_att, size_t nb_m, size_t nb_n,
                 const AttributeCols_Reorder& ac, const ObjectRows_Reorder& orow,
                 std::vector<int>* ei, std::vector<int>* ep, std::vector<uint64_t>* ib,
                 double* stats)
    : n_objects(n_obj), n_attributes(n_att), N_BLOCKS_M(nb_m), N_BLOCKS_N(nb_n),
      attr_cols(ac), obj_rows(orow),
      ext_i_out(ei), ext_p_out(ep), int_blocks_out(ib), canonicity_tests(stats) {}
};

// --- HELPERS ---
inline bool check_bit_M(const IntentAccumulator_Reorder& blocks, size_t k) {
  size_t block_idx = k >> 6;
  if (block_idx >= blocks.size()) return false;
  return (blocks[block_idx] & (1ULL << (k & 63))) != 0;
}

inline bool check_bit_N(const AttributeCols_Reorder& blocks, size_t attr_j, int obj_idx, size_t N_BLOCKS_N) {
  size_t idx = static_cast<size_t>(obj_idx);
  return (blocks[attr_j * N_BLOCKS_N + (idx >> 6)] & (1ULL << (idx & 63))) != 0;
}

inline bool check_bit_flat_block(const uint64_t* block_ptr, size_t k) {
  return (block_ptr[k >> 6] & (1ULL << (k & 63))) != 0;
}

// --- CORE RECURSIVO ---
void inclose_core_slim(int y,
                       const Extent_Reorder& extent,
                       IntentAccumulator_Reorder& intent,
                       InCloseContext& ctx,
                       int recursion_depth) {

  if (recursion_depth % 1000 == 0) Rcpp::checkUserInterrupt();

  ctx.ext_p_out->push_back(ctx.ext_i_out->size());
  ctx.ext_i_out->insert(ctx.ext_i_out->end(), extent.begin(), extent.end());
  ctx.int_blocks_out->insert(ctx.int_blocks_out->end(), intent.begin(), intent.end());

  Extent_Reorder child_extent;
  child_extent.reserve(extent.size());
  IntentAccumulator_Reorder child_intent(ctx.N_BLOCKS_M);

  for (int j = y + 1; j < ctx.n_attributes; j++) {
    if (check_bit_M(intent, static_cast<size_t>(j))) continue;

    child_extent.clear();
    for (int obj_idx : extent) {
      if (check_bit_N(ctx.attr_cols, static_cast<size_t>(j), obj_idx, ctx.N_BLOCKS_N)) {
        child_extent.push_back(obj_idx);
      }
    }

    if (child_extent.empty()) continue;

    for(size_t k = 0; k < ctx.N_BLOCKS_M; ++k) {
      uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
      size_t block_offset = k * ctx.n_objects;
      for (int obj_idx : child_extent) {
        intent_k &= ctx.obj_rows[block_offset + obj_idx];
      }
      child_intent[k] = intent_k;
    }

    (*ctx.canonicity_tests) += 1.0;
    bool is_canonical = true;
    size_t j_sz = static_cast<size_t>(j);
    size_t j_block_idx = j_sz >> 6;
    size_t j_bit_idx = j_sz & 63;

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
      inclose_core_slim(j, child_extent, child_intent, ctx, recursion_depth + 1);
    }
  }
}

// [[Rcpp::export]]
List InClose_Reorder(IntegerVector sp_i,
                     IntegerVector sp_p,
                     IntegerVector dim,
                     bool verbose = false) {

  // Recibimos VECTORES PUROS. Nada de S4, nada de slots, nada de RObject.
  // Esto es a prueba de balas.

  int n_objects = dim[0];
  int n_attributes = dim[1];

  if (n_objects <= 0 || n_attributes <= 0) return List::create();

  // --- Validación de Seguridad (Sanity Check) ---
  if (sp_p.size() != n_attributes + 1) stop("Invalid p vector length");

  // Copia segura a C++ STL para aislar memoria de R
  std::vector<int> sp_i_vec(sp_i.begin(), sp_i.end());
  std::vector<int> sp_p_vec(sp_p.begin(), sp_p.end());

  // Validación de índices (evita el segfault 0x48...)
  for(int r : sp_i_vec) {
    if (r < 0 || r >= n_objects) stop("Row index out of bounds");
  }

  const size_t N_BLOCKS_M = (static_cast<size_t>(n_attributes) + 63) / 64;
  const size_t N_BLOCKS_N = (static_cast<size_t>(n_objects) + 63) / 64;

  // Soporte
  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) attr_support[c] = {sp_p_vec[c+1] - sp_p_vec[c], c};
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    new_to_old_attr[j] = attr_support[j].second;
    old_to_new_attr[attr_support[j].second] = j;
  }

  AttributeCols_Reorder attr_cols(n_attributes * N_BLOCKS_N, 0);
  ObjectRows_Reorder obj_rows(N_BLOCKS_M * n_objects, 0);

  for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
    int start = sp_p_vec[c_orig];
    int end = sp_p_vec[c_orig + 1];
    if (start >= end) continue;

    int c_new = old_to_new_attr[c_orig];
    size_t c_new_sz = static_cast<size_t>(c_new);
    size_t block_idx_m = c_new_sz >> 6; size_t bit_idx_m = c_new_sz & 63;

    for (int k = start; k < end; ++k) {
      int r = sp_i_vec[k];
      if (r >= n_objects) continue;

      size_t r_sz = static_cast<size_t>(r);
      size_t block_idx_n = r_sz >> 6; size_t bit_idx_n = r_sz & 63;

      attr_cols[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);
      obj_rows[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
    }
  }

  sp_i_vec.clear(); sp_i_vec.shrink_to_fit();
  sp_p_vec.clear(); sp_p_vec.shrink_to_fit();

  // Algoritmo (sin cambios)
  double canonicity_tests = 0;
  std::vector<int> ext_i_out;
  std::vector<int> ext_p_out;
  std::vector<uint64_t> int_blocks_out;

  size_t est = (size_t)(n_objects * n_attributes) / 10;
  if (est < 1000) est = 1000;
  try {
    ext_i_out.reserve(est * n_objects / 5);
    ext_p_out.reserve(est + 1);
    int_blocks_out.reserve(est * N_BLOCKS_M);
  } catch(...) {}

  std::vector<uint64_t> m_prime(N_BLOCKS_N, 0xFFFFFFFFFFFFFFFF);
  for (int j = 0; j < n_attributes; j++) {
    size_t offset = j * N_BLOCKS_N;
    for(size_t k = 0; k < N_BLOCKS_N; ++k) m_prime[k] &= attr_cols[offset + k];
  }
  bool m_prime_is_empty = true;
  for(size_t k = 0; k < N_BLOCKS_N; ++k) if (m_prime[k] != 0) { m_prime_is_empty = false; break; }

  if (m_prime_is_empty) {
    ext_p_out.push_back(ext_i_out.size());
    int_blocks_out.insert(int_blocks_out.end(), N_BLOCKS_M, 0xFFFFFFFFFFFFFFFF);
  }

  Extent_Reorder initial_extent; initial_extent.reserve(n_objects);
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);
  IntentAccumulator_Reorder initial_intent(N_BLOCKS_M);
  for(size_t k = 0; k < N_BLOCKS_M; ++k) {
    uint64_t intent_k = 0xFFFFFFFFFFFFFFFF;
    size_t block_offset = k * n_objects;
    for (int obj_idx : initial_extent) intent_k &= obj_rows[block_offset + obj_idx];
    initial_intent[k] = intent_k;
  }

  InCloseContext ctx(n_objects, n_attributes, N_BLOCKS_M, N_BLOCKS_N, attr_cols, obj_rows,
                     &ext_i_out, &ext_p_out, &int_blocks_out, &canonicity_tests);

  inclose_core_slim(-1, initial_extent, initial_intent, ctx, 0);

  // Empaquetado
  ext_p_out.push_back(ext_i_out.size());
  int n_concepts = ext_p_out.size() - 1;

  S4 extents_S4("dgCMatrix");
  extents_S4.slot("i") = wrap(ext_i_out);
  extents_S4.slot("p") = wrap(ext_p_out);
  extents_S4.slot("x") = NumericVector(ext_i_out.size(), 1.0);
  extents_S4.slot("Dim") = IntegerVector::create(n_objects, n_concepts);

  std::vector<int> int_i_vec;
  std::vector<int> int_p_vec; int_p_vec.reserve(n_concepts + 1); int_p_vec.push_back(0);
  if (n_concepts > 0) int_i_vec.reserve(n_concepts * n_attributes / 2);

  for(int c = 0; c < n_concepts; ++c) {
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

  S4 intents_S4("dgCMatrix");
  intents_S4.slot("i") = wrap(int_i_vec);
  intents_S4.slot("p") = wrap(int_p_vec);
  intents_S4.slot("x") = NumericVector(int_i_vec.size(), 1.0);
  intents_S4.slot("Dim") = IntegerVector::create(n_attributes, n_concepts);

  return List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = n_concepts,
    _["tests"] = canonicity_tests,
    _["att_intents"] = 0
  );
}
