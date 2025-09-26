#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include "aux_functions.h"
#include <vector>
#include <algorithm>

using namespace Rcpp;

// --- VERSIÓN BINARIA DE IS_CANONICAL (CORREGIDA) ---
// Ahora acepta el extent como un std::vector<int> para coincidir con la llamada.
bool is_canonical_binary(double* I,
                         const std::vector<int>& A, // <-- CAMBIO: Acepta std::vector
                         const double* B_parent,
                         int n_objects,
                         int n_attributes,
                         int j,
                         double* all_att_intents) {

  for (int k = 0; k < j; k++) {
    if (B_parent[k] == 1.0) continue;

    (*all_att_intents) += 1.0;
    bool has_all_attributes = true;

    // --- CAMBIO: Itera sobre el std::vector directamente ---
    for (int obj_idx : A) {
      if (I[k * n_objects + obj_idx] == 0) {
        has_all_attributes = false;
        break;
      }
    }

    if (has_all_attributes) {
      return false;
    }
  }

  return true;
}

void inclose_binary_core(double* I,
                         int y,
                         int n_objects,
                         int n_attributes,
                         std::vector<int>& extent,
                         std::vector<double>& intent,
                         SparseVector* extents,
                         DoubleArray* intents,
                         double* canonicity_tests,
                         double* all_att_intents) {

  SparseVector current_extent;
  initVector(&current_extent, n_objects);
  for(int obj : extent) {
    insertArray(&current_extent.i, obj);
    insertArray(&current_extent.x, 1.0);
  }
  add_column(extents, current_extent);
  freeVector(&current_extent);

  for(double val : intent) {
    insertArray(intents, val);
  }

  for (int j = y + 1; j < n_attributes; j++) {
    if (intent[j] == 1.0) continue;

    std::vector<int> new_extent;
    for (int obj_idx : extent) {
      if (I[j * n_objects + obj_idx] == 1.0) {
        new_extent.push_back(obj_idx);
      }
    }

    // Si el extent es vacío, no generará más conceptos.
    if (new_extent.empty()) continue;

    if (new_extent.size() == extent.size()) {
      continue;
    }

    std::vector<double> new_intent = intent;
    new_intent[j] = 1.0;

    for (int attr = 0; attr < n_attributes; ++attr) {
      if (new_intent[attr] == 0) {
        bool has_all = true;
        for (int obj : new_extent) {
          if (I[attr * n_objects + obj] == 0) {
            has_all = false;
            break;
          }
        }
        if (has_all) {
          new_intent[attr] = 1.0;
        }
      }
    }

    (*canonicity_tests) += 1.0;
    // --- La llamada ahora es correcta ---
    if (is_canonical_binary(I, new_extent, intent.data(), n_objects, n_attributes, j, all_att_intents)) {
      inclose_binary_core(I, j, n_objects, n_attributes,
                          new_extent, new_intent,
                          extents, intents,
                          canonicity_tests, all_att_intents);
    }
  }
}

// [[Rcpp::export]]
List InClose_binary(NumericMatrix I,
                    StringVector attrs,
                    bool verbose = false) {

  Timer timer;
  timer.step("start");

  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  double canonicity_tests = 0;
  double all_att_intents = 0;

  SparseVector extents;
  DoubleArray intents;
  initVector(&extents, n_objects);
  initArray(&intents, n_attributes);

  std::vector<double> initial_intent(n_attributes, 0.0);
  std::vector<int> initial_extent;
  for(int i = 0; i < n_objects; ++i) initial_extent.push_back(i);

  for (int attr = 0; attr < n_attributes; ++attr) {
    bool has_all = true;
    for (int obj : initial_extent) {
      if (I[attr * n_objects + obj] == 0) {
        has_all = false;
        break;
      }
    }
    if (has_all) {
      initial_intent[attr] = 1.0;
    }
  }

  inclose_binary_core(I.begin(), -1, n_objects, n_attributes,
                      initial_extent, initial_intent,
                      &extents, &intents,
                      &canonicity_tests, &all_att_intents);

                      S4 intents_S4 = DenseArrayToS4(intents, n_attributes);
                      S4 extents_S4 = SparseToS4_fast(extents);

                      freeVector(&extents);
                      freeArray(&intents);

                      timer.step("end");

                      List res = List::create(
                        _["intents"] = intents_S4,
                        _["extents"] = extents_S4,
                        _["total"] = (intents.used > 0) ? (intents.used / n_attributes) : 0,
                        _["tests"] = canonicity_tests,
                        _["att_intents"] = all_att_intents,
                        _["timer"] = timer);

                      return res;
}
