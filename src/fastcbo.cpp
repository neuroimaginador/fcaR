#include "aux_functions.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>

using namespace Rcpp;

// Fast Close By One (FCbO) Algorithm
//
// References:
// Outrata, J., & Vychodil, V. (2012). Fast algorithm for computing fixpoints of
// Galois connections induced by object-attribute relational data.
//
// This algorithm is a recursive version of Close By One (CbO).
// It generates all formal concepts of a formal context.
// The key idea is to use a "canonicity test" to avoid generating the same
// concept multiple times. A concept (A, B) is canonical if B contains no
// attributes $j < y$ (where $y$ is the current attribute index) that are not
// already in B. Or more precisely, for the recursion: We iterate over
// attributes $j > y$. We form a candidate intent $D = (B \cup \{j\})''$. If $D
// \cap \{0, \dots, j-1\} = B \cap \{0, \dots, j-1\}$, then $D$ is canonical and
// we recurse.

void FuzzyFastGenerateFrom4(
    double *I, int n_objects, int n_attributes, int n_grades,
    StringVector attrs, double *grades_set, SparseVector *extents,
    SparseVector *intents,
    const SparseVector &qA, // Use reference for efficiency
    const SparseVector &qB, // Use reference for efficiency
    int id_col, int y,
    const double *Ny, // Parent matrix (read-only)
    double *closure_count, double *intent_count, double *total, int depth,
    bool verbose) {

  SparseVector A, B;
  initVector(&A, qA.length);
  initVector(&B, qB.length);
  get_column(&A, qA, id_col);
  get_column(&B, qB, id_col);

  // Add <A, B> to list of concepts.
  add_column(extents, A);
  add_column(intents, B);
  (*total) += 1.0;

  if ((cardinal(B) == n_attributes) || (y >= n_attributes)) {
    freeVector(&A);
    freeVector(&B);
    return;
  }

  double *Bv = (double *)calloc(B.length, sizeof(double));
  double sumB = 0.0;
  for (size_t i = 0; i < B.i.used; i++) {
    Bv[B.i.array[i]] = B.x.array[i];
    sumB += B.x.array[i];
  }

  // Back to the robust and correct queue system
  // We queue the valid children to process them after iterating through all
  // candidates in this level
  SparseVector queue_A, queue_B;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);
  IntArray queue_y;
  initArray(&queue_y, n_attributes * n_grades);

  SparseVector foo, D;
  initVector(&foo, n_objects);
  initVector(&D, n_attributes);

  // --- KEY FIX ---
  // Mj is a local copy of the parent matrix (Ny).
  // Changes in Mj will not affect other branches (siblings).
  // This matrix M stores the computed closures values for previous attributes,
  // used for the canonicity test.
  double *Mj = (double *)malloc((size_t)n_attributes * n_attributes * n_grades *
                                sizeof(double));
  memcpy(Mj, Ny,
         (size_t)n_attributes * n_attributes * n_grades * sizeof(double));

  if ((sumB < n_attributes) && (y < n_attributes)) {
    for (int j = y + 1; j < n_attributes; j++) {
      double B_j = Bv[j];
      if (B_j == 1)
        continue;

      for (int g_idx = 0; g_idx < n_grades; g_idx++) {
        double g = grades_set[g_idx];

        if (B_j >= g) {
          // Skip grades that are already covered by the current attribute value
          // in B (optimization: avoid redundant calculations)
          while (g_idx < n_grades - 1 && g <= B_j) {
            g_idx++;
            g = grades_set[g_idx];
          }
          if (B_j >= g)
            continue;
        }

        // First partial canonicity check
        // We check if the attribute j (with grade g) is already implied by
        // previous attributes $s < j$. If so, it would have been generated
        // earlier, so we skip it.
        if (j > 0) {
          int s = 0;
          for (s = 0; s < j; s++) {
            // Use Ny (parent matrix) for the check
            if (Ny[(size_t)g_idx * n_attributes * n_attributes +
                   (size_t)j * n_attributes + s] > Bv[s])
              break;
          }
          if (s < j)
            continue;
        }

        reinitVector(&foo);
        cloneVector(&foo, A);

        for (size_t id = 0; id < foo.i.used; id++) {
          int i = foo.i.array[id];
          double val = I[(size_t)j * n_objects + i];
          if (val < g) {
            foo.x.array[id] = std::min(foo.x.array[id], val);
          }
        }

        reinitVector(&D);
        compute_intent(&D, foo, I, n_objects, n_attributes);
        (*intent_count) += n_attributes;
        (*closure_count) += 1.0;

        double gv = get_element(D, j);

        if (gv != g) {
          // Advance grades if the computed value is higher than current g
          while (g_idx < n_grades - 1 && g < gv) {
            g_idx++;
            g = grades_set[g_idx];
          }
          if (g < gv)
            continue;
        }

        bool canonical = true;
        // Full canonicity check
        // Check if D contains any attribute s < j that is not in B (or has a
        // higher value than in B).
        if (j > 0) {
          size_t s_idx = 0;
          for (size_t is = 0; is < D.i.used; ++is) {
            int iD = D.i.array[is];
            if (iD >= j)
              break;
            if (Bv[iD] < D.x.array[is]) {
              canonical = false;
              break;
            }
          }
        }

        if (canonical) {
          add_column(&queue_A, foo);
          add_column(&queue_B, D);
          insertArray(&queue_y, j);
        } else {
          // Update our local copy of Mj, not the parent's
          // This updates the information about what this non-canonical closure
          // implies, which can be used to prune future candidates in this
          // branch.
          for (size_t idx = 0; idx < D.i.used; idx++) {
            size_t linear_index = (size_t)g_idx * n_attributes * n_attributes +
                                  (size_t)j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];
          }
        }
      }
    }
  }

  for (size_t cols = 0; cols < queue_y.used; cols++) {
    FuzzyFastGenerateFrom4(
        I, n_objects, n_attributes, n_grades, attrs, grades_set, extents,
        intents, queue_A, queue_B, cols, queue_y.array[cols],
        Mj, // Pass our modified Mj to children
        closure_count, intent_count, total, depth + 1, verbose);
  }

  // Free all local memory
  freeVector(&queue_A);
  freeVector(&queue_B);
  freeArray(&queue_y);
  freeVector(&foo);
  freeVector(&D);
  free(Mj);
  free(Bv);
  freeVector(&A);
  freeVector(&B);
}

// [[Rcpp::export]]
List FuzzyFCbO(NumericMatrix I, NumericVector grades_set, StringVector attrs,
               String connection = "standard", String name = "Zadeh",
               bool verbose = false) {

  Timer timer;
  timer.step("start");

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  double closure_count = 0;
  double intent_count = 0;
  double total = 0;

  SparseVector B; // Empty intent
  initVector(&B, n_attributes);

  SparseVector A;
  initVector(&A, n_objects);
  compute_extent(&A, B, I.begin(), n_objects, n_attributes);
  freeVector(&B);

  SparseVector C;
  initVector(&C, n_attributes);
  compute_intent(&C, A, I.begin(), n_objects, n_attributes);
  intent_count += n_attributes;

  SparseVector intents;
  SparseVector extents;
  initVector(&intents, n_attributes);
  initVector(&extents, n_objects);

  // The original matrix, empty, passed to the first call
  double *Ny = (double *)calloc((size_t)n_attributes * n_attributes * n_grades,
                                sizeof(double));

  // The first recursion call
  FuzzyFastGenerateFrom4(I.begin(), n_objects, n_attributes, n_grades, attrs,
                         grades_set.begin(), &extents, &intents, A, C, 0, -1,
                         Ny, &closure_count, &intent_count, &total, 0, verbose);

  free(Ny);

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&extents);

  timer.step("end");

  List res =
      List::create(_["intents"] = intents_S4, _["extents"] = extents_S4,
                   _["total"] = total, _["closure_count"] = closure_count,
                   _["intent_count"] = intent_count, _["timer"] = timer);

  return res;
}
