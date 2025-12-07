#include "aux_functions.h"
#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>

using namespace Rcpp;

// In-Close Algorithm
//
// References:
// Andrews, S. (2009). In-Close, a fast algorithm for computing formal concepts.
// Andrews, S. (2011). In-Close2, a high-performance formal concept miner.
//
// This algorithm is also a Close By One variant, optimized for performance.
// It iterates through attributes and recursively builds concepts.
// It maintains a pair (A, B) where A is the extent and B is the intent.
// The "next" attribute j is added to the intent, and the extent is refined
// (intersection).
//
// Canonicity test:
// Like Next Closure and FCbO, In-Close ensures that each concept is generated
// exactly once. A concept (A, B) is canonical if B contains no attributes $k <
// j$ (where j is the attribute used to generate this branch) that are not
// already in the parent concept's intent.
//
bool is_canonical5(double *I, SparseVector A, DoubleArray B, int n_objects,
                   int n_attributes, int r, int rnew, int j, int *kk, int gr,
                   double *canonicity_tests, double *all_att_intents,
                   bool verbose) {

  if (verbose)
    Rcout << "    |- Canonicity:" << std::endl;

  bool res = true;
  (*kk) = -1;
  (*canonicity_tests) += 1.0;

  if (j > 0) {

    for (int k = 0; k < j; k++) {

      // If attribute k is already in B (value >= 1), we don't need to check it.
      if (B.array[r * n_attributes + k] >= 1)
        continue;

      // Calculate the value of attribute k for the extent A.
      // This is effectively calculating the closure of A with respect to
      // attribute k. $gv = \bigwedge_{x \in A} I(x, k)$
      double gv = 1;
      for (int i = 0; i < A.i.used; i++) {

        int n = A.i.array[i];

        if (A.x.array[i] == 0)
          continue;

        double val =
            (A.x.array[i] <= I[k * n_objects + n]) ? 1.0 : I[k * n_objects + n];

        if (val < gv)
          gv = val;

        // Optimization: if gv drops to the value already in B, we stop.
        // if (gv == 0) break;
        if (gv == B.array[r * n_attributes + k])
          break;
      }

      if (verbose)
        Rcout << "     gv raw = " << gv << std::endl;

      (*all_att_intents) += 1.0;

      if (verbose)
        Rcout << "     gv = " << gv << ", B[" << k
              << "] = " << B.array[r * n_attributes + k];

      // If the computed value for attribute k is greater than what is in B,
      // it means attribute k implies something new that should have been
      // processed earlier. Hence, this concept generation path is not
      // canonical.
      if (gv > B.array[r * n_attributes + k]) {

        if (verbose)
          Rcout << " => Not canonical" << std::endl;

        (*kk) = k;
        return false;
        break;
      }

      if (verbose)
        Rcout << " => Canonical" << std::endl;
    }
  }

  return res;
}

void inclose_core(double *I, double *grades_set, int r, int y, int gr,
                  int n_objects, int n_attributes, int n_grades,
                  SparseVector *A, DoubleArray *B, const double *P1,
                  const bool *G1, int *rnew, double *canonicity_tests,
                  double *partial_canonicity_tests, double *all_att_intents,
                  int depth, bool *add_empty_extent, bool verbose) {

  if (verbose) {

    Rcout << std::endl
          << "*****" << std::endl
          << "DEPTH = " << depth << std::endl;
    Rcout << "parent = " << r << std::endl;

    Rcout << "parent intent = ";
    for (int i = 0; i < n_attributes; i++) {

      Rcout << B->array[r * n_attributes + i] << " ";
    }
    Rcout << std::endl;
    Rcout << "parent P = ";

    for (int k = 0; k < n_attributes; k++) {

      Rcout << P1[k] << " ";
    }

    Rcout << std::endl;
  }

  if (y > n_attributes - 1)
    return;

  int kk;

  IntArray gchildren, jchildren, rchildren;
  initArray(&gchildren, n_attributes * n_grades);
  initArray(&jchildren, n_attributes * n_grades);
  initArray(&rchildren, n_attributes * n_grades);

  double *P = (double *)calloc(n_attributes, sizeof(double));
  memcpy(P, P1, n_attributes * sizeof(double));
  bool *G = (bool *)calloc(n_attributes * n_grades, sizeof(bool));
  memcpy(G, G1, n_attributes * n_grades * sizeof(bool));

  double g, gv;
  SparseVector Ar, Arnew;
  initVector(&Ar, n_objects);
  initVector(&Arnew, n_objects);

  // Run through the remaining attributes starting from y + 1
  for (int j = y + 1; j < n_attributes; j++) {

    // Loop over grades in order
    for (int g_idx = 0; g_idx < n_grades; g_idx++) {

      // Rcout << "G(" << j << "," << g_idx << ") = " << G[n_attributes * g_idx
      // + j] << std::endl;

      if (!G[n_attributes * g_idx + j])
        continue;

      g = grades_set[g_idx];

      if (verbose)
        Rcout << "Testing j = " << j << " and g = " << g << std::endl;

      if ((P[j] <= g) && (P[j] > 0)) {

        if (verbose) {

          Rcout << "     Comes from empty intersection." << std::endl;
          Rcout << "     P = ";

          for (int k = 0; k < n_attributes; k++) {

            Rcout << P[k] << " ";
          }

          Rcout << std::endl;
        }

        g_idx = n_grades;
        continue;
      }

      // if B(j) < g i.e., B\cap m_j \subsetneq g/m_j
      // That is, the current intent B does not already contain attribute j with
      // at least grade g.
      if (B->array[r * n_attributes + j] < g) {

        reinitVector(&Arnew);
        reinitVector(&Ar);

        get_column(&Arnew, *A, r);

        bool is_parent = true;

        // Intersect A with {g/j}^{\downarrow}
        // Arnew is "C" in the pseudocode: $C = A \cap \{x \in G \mid I(x, j)
        // \ge g\}$
        double sumC = 0.0;
        for (int id = 0; id < Arnew.i.used; id++) {

          int i = Arnew.i.array[id];
          double val = (I[j * n_objects + i] >= g) ? 1.0 : I[j * n_objects + i];

          if (val > Arnew.x.array[id]) {

            val = Arnew.x.array[id];
          }

          // This allows to check if C = A
          if (val < Arnew.x.array[id])
            is_parent = false;

          Arnew.x.array[id] = val;
          sumC += val;
        }

        if (verbose) {

          Rcout << "    - Studying extent: ";

          int count = 0;
          for (int z = 0; z < n_objects; z++) {

            if ((count < Arnew.i.used) && (z == Arnew.i.array[count])) {

              Rcout << Arnew.x.array[count] << " ";
              count++;

            } else {

              Rcout << "0 ";
            }
          }
          Rcout << std::endl;
        }

        if (sumC == 0) {

          (*add_empty_extent) = true;

          P[j] = (P[j] > 0) ? ((P[j] > g) ? g : P[j]) : g;
          // if (P[j] == 0) {
          //
          //   P[j] = g;
          //
          // } else {
          //
          //   P[j] = (P[j] > g) ? g : P[j];
          //
          // }
          // P[j] = g;

          if (verbose) {

            Rcout << "     Detected empty intersection." << std::endl;
            Rcout << "     P = ";

            for (int k = 0; k < n_attributes; k++) {

              Rcout << P[k] << " ";
            }

            Rcout << std::endl;
          }

          g_idx = n_grades;
          break;
        }

        // gv is C^{\uparrow}\cap m_j
        // Compute closure of the new extent (Arnew) with respect to attribute
        // j.
        gv = 1;
        for (int i = 0; i < Arnew.i.used; i++) {

          int n = Arnew.i.array[i];
          double val = (Arnew.x.array[i] <= I[j * n_objects + n])
                           ? 1.0
                           : I[j * n_objects + n];

          if (val < gv)
            gv = val;
        }
        (*partial_canonicity_tests) += 1.0;
        (*all_att_intents) += 1.0;

        // If C^{\uparrow}\cap m_j != g/m_j, next value of grade
        if (gv != g) {

          // Gdown[j] = g; //(Gdown[j] > g) ? g : Gdown[j];
          // Gup[j] = gv;

          // We move forward several steps
          while (g < gv) {

            G[n_attributes * g_idx + j] = false;
            g_idx++;
            g = grades_set[g_idx];
          }

          if (verbose) {

            Rcout << "     Moving to g = " << g << std::endl;
          }

          // continue; //??
        }

        // If C == A
        if (is_parent) {

          // Update B (the parent intent)
          B->array[r * n_attributes + j] = gv;

          if (verbose) {

            Rcout << "     Equal to parent. Adding attribute " << j
                  << " with grade " << g << std::endl;
            Rcout << "     New intent = ";

            for (int i = 0; i < n_attributes; i++) {

              Rcout << B->array[r * n_attributes + i] << " ";
            }
            Rcout << std::endl;
          }

        } else {

          if (is_canonical5(I, Arnew, *B, n_objects, n_attributes, r, (*rnew),
                            j, &kk, g_idx, canonicity_tests, all_att_intents,
                            verbose)) {

            if (verbose) {

              Rcout << "     Added extent " << (*rnew) << ": ";

              int count = 0;
              for (int z = 0; z < n_objects; z++) {

                if ((count < Arnew.i.used) && (z == Arnew.i.array[count])) {

                  Rcout << Arnew.x.array[count] << " ";
                  count++;

                } else {

                  Rcout << "0 ";
                }
              }
              Rcout << std::endl;
            }

            insertArray(&jchildren, j);
            insertArray(&gchildren, g_idx);
            insertArray(&rchildren, (*rnew));

            for (int i = 0; i < n_attributes; i++) {

              insertArray(B, B->array[r * n_attributes + i]);
            }

            B->array[(*rnew) * n_attributes + j] = gv;

            if (verbose) {

              Rcout << "     With initial intent: ";

              for (int i = 0; i < n_attributes; i++) {

                Rcout << B->array[(*rnew) * n_attributes + i] << " ";
              }
              Rcout << std::endl;
            }

            add_column(A, Arnew);

            (*rnew)++;

          } else {

            if (kk <= y)
              P[j] = (P[j] > 0) ? ((P[j] > g) ? g : P[j]) : g;
            // P[j] = (P[j] < g) ? g : P[j];
            if (verbose)
              Rcout << "     Rejected extent" << std::endl;
          }
        }
      }
    }
  }

  freeVector(&Ar);
  freeVector(&Arnew);

  // Loop over children
  for (int i = 0; i < jchildren.used; i++) {

    inclose_core(I, grades_set, rchildren.array[i], jchildren.array[i],
                 gchildren.array[i], n_objects, n_attributes, n_grades, A, B, P,
                 G, rnew, canonicity_tests, partial_canonicity_tests,
                 all_att_intents, depth + 1, add_empty_extent, verbose);
  }

  freeArray(&gchildren);
  freeArray(&jchildren);
  freeArray(&rchildren);
  free(P);
  free(G);
}

// [[Rcpp::export]]
List InClose(NumericMatrix I, NumericVector grades_set, StringVector attrs,
             String connection = "standard", String name = "Zadeh",
             bool verbose = false) {

  Timer timer;
  timer.step("start");

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  double canonicity_tests = 0;
  double partial_canonicity_tests = 0;
  double all_att_intents = 0;

  SparseVector A;
  DoubleArray B;
  initVector(&A, n_objects);
  initArray(&B, n_attributes);

  double *P = (double *)calloc(n_attributes, sizeof(double));
  bool *G = (bool *)calloc(n_attributes * n_grades, sizeof(bool));
  memset(G, true, n_attributes * n_grades);
  // initArray(&P, n_attributes);

  bool add_empty_extent = false;

  // First extent
  for (int obj = 0; obj < n_objects; obj++) {

    insertArray(&(A.i), obj);
    insertArray(&(A.x), 1.0);
  }
  insertArray(&(A.p), 0);
  insertArray(&(A.p), n_objects);

  for (int att = 0; att < n_attributes; att++) {

    insertArray(&B, 0.0);
    // insertArray(&P, 0.0);
  }

  int rnew = 1;

  inclose_core(I.begin(), grades_set.begin(), 0, -1, n_grades, n_objects,
               n_attributes, n_grades, &A, &B, P, G, &rnew, &canonicity_tests,
               &partial_canonicity_tests, &all_att_intents, 0,
               &add_empty_extent, verbose);

  if (add_empty_extent) {

    // Rcout << "Adding" << std::endl;

    // Empty extent
    int lastp = A.p.array[A.p.used - 1];
    // Rcout << "lastp = " << lastp << std::endl;
    // Rcout << "A.p.used = " << A.p.used << std::endl;

    insertArray(&(A.p), lastp);

    // Rcout << "A.p.used = " << A.p.used << std::endl;

    for (int att = 0; att < n_attributes; att++) {

      insertArray(&B, 1.0);
    }
    rnew++;
  }

  S4 intents_S4 = DenseArrayToS4(B, n_attributes);
  S4 extents_S4 = SparseToS4_fast(A);

  freeVector(&A);
  freeArray(&B);
  free(P);
  free(G);

  timer.step("end");

  List res = List::create(
      _["intents"] = intents_S4, _["extents"] = extents_S4, _["total"] = rnew,
      _["tests"] = canonicity_tests, _["partial"] = partial_canonicity_tests,
      _["att_intents"] = all_att_intents, _["timer"] = timer);

  return res;
}
