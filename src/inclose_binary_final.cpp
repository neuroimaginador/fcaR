#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include <vector>
#include <algorithm>
#include <utility>

using namespace Rcpp;

// --- TIPOS ---
using Extent_Reorder = std::vector<int>;
using AttributeCols_Reorder = std::vector<uint64_t>;
using ObjectRows_Reorder = std::vector<uint64_t>;
using IntentAccumulator_Reorder = std::vector<uint64_t>;

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

  // DEBUG: Solo imprimir en niveles superficiales para no saturar log
  if (recursion_depth < 2) {
    Rcpp::Rcout << "[DEBUG] Recursion Depth: " << recursion_depth << " Y: " << y << std::endl;
  }

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
List InClose_Reorder(RObject I, bool verbose = false) {
  Rcpp::Rcout << "[DEBUG] Entered InClose_Reorder" << std::endl;
  Timer timer;
  timer.step("start_reorder_setup");

  int n_objects = 0;
  int n_attributes = 0;
  std::vector<int> sp_i_vec;
  std::vector<int> sp_p_vec;

  Rcpp::Rcout << "[DEBUG] Extracting Data..." << std::endl;
  if (I.isS4()) {
    S4 mat(I);
    if (!mat.hasSlot("i") || !mat.hasSlot("p") || !mat.hasSlot("Dim")) stop("Invalid S4 matrix.");
    IntegerVector dim = mat.slot("Dim");
    n_objects = dim[0]; n_attributes = dim[1];
    IntegerVector i_R = mat.slot("i");
    IntegerVector p_R = mat.slot("p");
    sp_i_vec.assign(i_R.begin(), i_R.end());
    sp_p_vec.assign(p_R.begin(), p_R.end());
  } else {
    IntegerMatrix mat = as<IntegerMatrix>(I);
    n_objects = mat.nrow(); n_attributes = mat.ncol();
    sp_p_vec.resize(n_attributes + 1, 0);
    for (int c = 0; c < n_attributes; ++c) {
      int count = 0;
      for (int r = 0; r < n_objects; ++r) {
        if (mat(r, c) != 0) { sp_i_vec.push_back(r); count++; }
      }
      sp_p_vec[c+1] = sp_p_vec[c] + count;
    }
  }
  Rcpp::Rcout << "[DEBUG] Data Extracted. Objects: " << n_objects << " Attributes: " << n_attributes << std::endl;

  if (n_objects == 0 || n_attributes == 0) return List::create();

  const size_t N_BLOCKS_M = (static_cast<size_t>(n_attributes) + 63) / 64;
  const size_t N_BLOCKS_N = (static_cast<size_t>(n_objects) + 63) / 64;

  Rcpp::Rcout << "[DEBUG] Sorting attributes..." << std::endl;
  std::vector<std::pair<int, int>> attr_support(n_attributes);
  for (int c = 0; c < n_attributes; ++c) attr_support[c] = {sp_p_vec[c+1] - sp_p_vec[c], c};
  std::sort(attr_support.begin(), attr_support.end());

  std::vector<int> new_to_old_attr(n_attributes);
  std::vector<int> old_to_new_attr(n_attributes);
  for (int j = 0; j < n_attributes; ++j) {
    new_to_old_attr[j] = attr_support[j].second;
    old_to_new_attr[attr_support[j].second] = j;
  }

  Rcpp::Rcout << "[DEBUG] Building Bitsets..." << std::endl;
  AttributeCols_Reorder attr_cols(n_attributes * N_BLOCKS_N, 0);
  ObjectRows_Reorder obj_rows(N_BLOCKS_M * n_objects, 0);

  for (int c_orig = 0; c_orig < n_attributes; ++c_orig) {
    int start = sp_p_vec[c_orig];
    int end = sp_p_vec[c_orig + 1];
    if (start == end) continue;
    int c_new = old_to_new_attr[c_orig];
    size_t c_new_sz = static_cast<size_t>(c_new);
    size_t block_idx_m = c_new_sz >> 6; size_t bit_idx_m = c_new_sz & 63;

    for (int k = start; k < end; ++k) {
      int r = sp_i_vec[k];
      size_t r_sz = static_cast<size_t>(r);
      size_t block_idx_n = r_sz >> 6; size_t bit_idx_n = r_sz & 63;
      attr_cols[c_new_sz * N_BLOCKS_N + block_idx_n] |= (1ULL << bit_idx_n);
      obj_rows[block_idx_m * n_objects + r_sz] |= (1ULL << bit_idx_m);
    }
  }

  sp_i_vec.clear(); sp_i_vec.shrink_to_fit();
  sp_p_vec.clear(); sp_p_vec.shrink_to_fit();

  double canonicity_tests = 0;
  std::vector<int> ext_i_out; ext_i_out.reserve(n_objects * 5);
  std::vector<int> ext_p_out; ext_p_out.reserve(1000);
  std::vector<uint64_t> int_blocks_out; int_blocks_out.reserve(1000 * N_BLOCKS_M);

  // Bottom check
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

  Rcpp::Rcout << "[DEBUG] Initializing Context..." << std::endl;
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

  Rcpp::Rcout << "[DEBUG] Starting Recursion..." << std::endl;
  timer.step("start_reorder_recursion");
  inclose_core_slim(-1, initial_extent, initial_intent, ctx, 0);
  timer.step("end_reorder_recursion");
  Rcpp::Rcout << "[DEBUG] Recursion Finished." << std::endl;

  // Empaquetado
  ext_p_out.push_back(ext_i_out.size());
  int n_concepts = ext_p_out.size() - 1;

  Rcpp::Rcout << "[DEBUG] Packaging Results: " << n_concepts << " concepts." << std::endl;

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

  timer.step("end_reorder_packaging");
  Rcpp::Rcout << "[DEBUG] Done." << std::endl;

  return List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = n_concepts,
    _["tests"] = canonicity_tests,
    _["att_intents"] = 0,
    _["timer"] = timer
  );
}
