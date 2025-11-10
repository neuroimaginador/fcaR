#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include "aux_functions.h"
#include "vector_operations.h"
#include <vector>
#include <algorithm>
#include <boost/dynamic_bitset.hpp>

using namespace Rcpp;

// =============================================================================
// --- FAST CLOSE-BY-ONE (BINARIO) v17-OPTIMIZADO (CORREGIDO) ---
// =============================================================================

// --- Definiciones de tipos (v15) ---
using Bitset = boost::dynamic_bitset<>;
using Extent_v_io = std::vector<int>; // Para E/S (v13)
using AttributeCols = std::vector<Bitset>; // Matriz (columnas)
using ObjectRows = std::vector<Bitset>;    // Matriz (filas)

// --- Helpers de E/S a Granel (v15 estables) ---
inline void ensureArray_double_v9(DoubleArray *a, size_t additional_size) {
  if (a->used + additional_size > a->size) {
    size_t newSize = (a->size + additional_size) * 1.5;
    if (newSize == 0) newSize = 1;
    double* tmp = (double *)realloc(a->array, newSize * sizeof(double));
    if (tmp == NULL) { Rcpp::stop("Failed to realloc memory (double)"); }
    a->array = tmp; a->size = newSize;
    for (size_t i = a->used; i < a->size; i++) { a->array[i] = 0; }
  }
}
inline void insertArray_bulk_double_v9(DoubleArray *a, double* buffer, size_t n) {
  ensureArray_double_v9(a, n);
  memcpy(&a->array[a->used], buffer, n * sizeof(double));
  a->used += n;
}
inline void ensureArray_int_v10(IntArray *a, size_t additional_size) {
  if (a->used + additional_size > a->size) {
    size_t newSize = (a->size + additional_size) * 1.5;
    if (newSize == 0) newSize = 1;
    int* tmp = (int *)realloc(a->array, newSize * sizeof(int));
    if (tmp == NULL) { Rcpp::stop("Failed to realloc memory (int)"); }
    a->array = tmp; a->size = newSize;
    for (size_t i = a->used; i < a->size; i++) { a->array[i] = 0; }
  }
}
inline void insertArray_bulk_int_v13(IntArray *a, const int* buffer, size_t n) {
  ensureArray_int_v10(a, n);
  memcpy(&a->array[a->used], buffer, n * sizeof(int));
  a->used += n;
}
// --- Fin de Helpers ---


/**
 * @brief Core recursivo de FastCbO (v17-dual).
 * Itera sobre objetos (g) y calcula intersecciones de intents.
 */
void fastcbo_core_v17(int g,
                      int n_objects,
                      int n_attributes,
                      Bitset& extent,      // Concepto actual (Bitset)
                      Bitset& intent,      // Concepto actual (Bitset)
                      const AttributeCols& attr_cols,
                      const ObjectRows& obj_rows,
                      SparseVector* extents_out, // Salida (dgCMatrix)
                      DoubleArray* intents_out, // Salida (Densa)
                      double* intent_buffer,       // Búfer E/S v9
                      double* extent_x_buffer,     // Búfer E/S v13 (pre-llenado)
                      Extent_v_io& extent_vec_io,   // Búfer E/S v13 (para 'i')
                      Bitset& canonical_helper,  // Búfer v16
                      double* canonicity_tests) {

  // 1. E/S Extent (v15: Copia Cero + X pre-llenado)
  extent_vec_io.clear();
  for (size_t obj = extent.find_first(); obj != Bitset::npos; obj = extent.find_next(obj)) {
    extent_vec_io.push_back(obj);
  }

  size_t n_added = extent_vec_io.size();
  if (n_added > 0) {
    insertArray_bulk_int_v13(&(extents_out->i), extent_vec_io.data(), n_added);
    insertArray_bulk_double_v9(&(extents_out->x), extent_x_buffer, n_added);
  }
  if (extents_out->p.used == 0) {
    insertArray(&(extents_out->p), 0);
  }
  int last_p = extents_out->p.array[extents_out->p.used - 1];
  insertArray(&(extents_out->p), last_p + n_added);

  // 2. E/S Intent (v9)
  for (int attr = 0; attr < n_attributes; ++attr) {
    intent_buffer[attr] = intent.test(attr) ? 1.0 : 0.0;
  }
  insertArray_bulk_double_v9(intents_out, intent_buffer, n_attributes);

  // 3. Asignación de memoria ÚNICA (v5)
  Bitset child_extent(n_objects);
  Bitset child_intent(n_attributes);

  // 4. Iterar sobre objetos 'i'
  for (int i = g + 1; i < n_objects; i++) {
    if (extent.test(i)) continue;

    // 5. Bucles Separados (v14)
    child_intent = intent & obj_rows[i];
    if (child_intent == intent) continue;

    child_extent.set();
    for (size_t attr = child_intent.find_first(); attr != Bitset::npos; attr = child_intent.find_next(attr)) {
      child_extent &= attr_cols[attr];
    }

    // 6. Test de Canonicidad (CbO)
    (*canonicity_tests) += 1.0;
    canonical_helper = child_extent & (~extent);
    size_t k = canonical_helper.find_first();

    // 7. Recursión
    if (k == i) {
      fastcbo_core_v17(i, n_objects, n_attributes,
                       child_extent, child_intent,
                       attr_cols, obj_rows,
                       extents_out, intents_out,
                       intent_buffer,
                       extent_x_buffer,
                       extent_vec_io,
                       canonical_helper,
                       canonicity_tests);
    }
  }
}


/**
 * @brief Wrapper Rcpp para el algoritmo Fast Close-by-One (CbO)
 * binario y optimizado (v17 - Corregido).
 */
// [[Rcpp::export]]
List FastCbO_binary(NumericMatrix I,
                        StringVector attrs,
                        bool verbose = false) {
  Timer timer;
  timer.step("start_cbo_setup");
  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  // 1. Construir representaciones duales de bitset (v2)
  AttributeCols attr_cols(n_attributes, Bitset(n_objects));
  ObjectRows obj_rows(n_objects, Bitset(n_attributes));
  for (int r = 0; r < n_objects; ++r) {
    for (int c = 0; c < n_attributes; ++c) {
      if (I(r, c) == 1.0) {
        attr_cols[c].set(r);
        obj_rows[r].set(c);
      }
    }
  }

  // 2. Inicializar contadores y estructuras de salida
  double canonicity_tests = 0;
  SparseVector extents_out; // Salida dgCMatrix
  DoubleArray intents_out;  // Salida Densa
  initVector(&extents_out, n_objects * 100);
  initArray(&intents_out, n_attributes * 1000);

  // 3. Búferes de E/S (v15)
  double* intent_io_buffer = (double*)calloc(n_attributes, sizeof(double));
  double* extent_x_io_buffer = (double*)calloc(n_objects, sizeof(double));
  if (intent_io_buffer == NULL || extent_x_io_buffer == NULL) {
    Rcpp::stop("Failed to calloc memory for I/O buffers");
  }
  for(int i = 0; i < n_objects; ++i) {
    extent_x_io_buffer[i] = 1.0;
  }

  Extent_v_io extent_vec_io;
  extent_vec_io.reserve(n_objects);

  // 4. Búferes de Algoritmo (v16)
  Bitset canonical_helper(n_objects);

  // 5. Calcular el concepto inicial (Bottom)
  // (A, B) = (Ø'', (Ø'')')

  // B = Ø' = M (todos los atributos)
  Bitset initial_intent(n_attributes);
  initial_intent.set();

  // A = B' = M' (Ø'')
  Bitset initial_extent(n_objects);
  initial_extent.set();
  for (size_t attr = initial_intent.find_first(); attr != Bitset::npos; attr = initial_intent.find_next(attr)) {
    initial_extent &= attr_cols[attr];
  }

  // B = A' = (Ø'')'
  initial_intent.set();
  for (size_t obj = initial_extent.find_first(); obj != Bitset::npos; obj = initial_extent.find_next(obj)) {
    initial_intent &= obj_rows[obj];
  }

  timer.step("start_cbo_recursion");

  // 6. Iniciar la recursión (versión CbO)
  fastcbo_core_v17(-1, n_objects, n_attributes,
                   initial_extent, initial_intent,
                   attr_cols, obj_rows,
                   &extents_out, &intents_out,
                   intent_io_buffer, // <-- CORREGIDO
                   extent_x_io_buffer,
                   extent_vec_io,
                   canonical_helper,
                   &canonicity_tests);

  timer.step("end_cbo_recursion");

  // 7. Empaquetar resultados
  S4 intents_S4 = DenseArrayToS4(intents_out, n_attributes);
  S4 extents_S4 = SparseToS4_fast(extents_out);

  // 8. Liberar búferes de E/S
  free(intent_io_buffer);
  free(extent_x_io_buffer);

  freeVector(&extents_out);
  freeArray(&intents_out);

  // (El timer.step("end_packaging") se omite, como pediste)

  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = (intents_out.used > 0) ? (intents_out.used / n_attributes) : 0,
    _["tests"] = canonicity_tests,
    _["timer"] = timer);
  return res;
}
