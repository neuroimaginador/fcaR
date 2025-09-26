#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include "aux_functions.h"

using namespace Rcpp;


void FuzzyFastGenerateFrom4(double* I,
                            // const SparseVector myI,
                            int n_objects,
                            int n_attributes,
                            int n_grades,
                            StringVector attrs,
                            LogicOperator tnorm,
                            LogicOperator implication,
                            double* grades_set,
                            SparseVector *extents,
                            SparseVector *intents,
                            SparseVector qA, // extent
                            SparseVector qB, //intent
                            int id_col,
                            int y,
                            // int gr,
                            const double* Ny,
                            double* closure_count,
                            double* intent_count,
                            double* total,
                            int depth,
                            bool verbose) {

  // Rcout << "DEPTH = " << depth << std::endl;
  // Rcout << "y = " << y << std::endl;

  // Se puede pasar la propia I a una lista de SparseVector
  // o el hacer el zadeh_I(g, I[, j]) en esa lista, para
  // evitar duplicar los cálculos

  SparseVector A, B;
  initVector(&A, qA.length);
  initVector(&B, qB.length);
  get_column(&A, qA, id_col);
  get_column(&B, qB, id_col);

  // Add <A, B> to list of concepts.
  add_column(extents, A);
  add_column(intents, B);
  (*total) += 1.0;

  if (verbose) {

    Rcout << std::endl << "*****" << std::endl << "DEPTH = " << depth << std::endl;
    Rcout << "parent intent = ";
    int count = 0;
    for (int z = 0; z < n_attributes; z++) {

      if ((count < B.i.used) && (z == B.i.array[count])) {

        Rcout << B.x.array[count] << " ";
        count++;

      } else {

        Rcout << "0 ";

      }

    }
    Rcout << std::endl;

  }


  if ((cardinal(B) == n_attributes) || (y >= n_attributes)) return;

  double* Bv = (double*)calloc(B.length, sizeof(double));
  double sumB = 0.0;
  for (int i = 0; i < B.i.used; i++) {

    Bv[B.i.array[i]] = B.x.array[i];
    sumB += B.x.array[i];

  }

  // Prepare to queue next recursion step.
  SparseVector queue_A, queue_B;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);

  IntArray queue_y; //, queue_gr;
  initArray(&queue_y, n_attributes * n_grades);
  // initArray(&queue_gr, n_attributes * n_grades);

  SparseVector foo, D;
  initVector(&foo, n_objects);
  initVector(&D, n_attributes);

  double gv;

  double* Mj = (double*)malloc(n_attributes * n_attributes * n_grades * sizeof(double));

  memcpy(Mj, Ny, n_attributes * n_attributes * n_grades * sizeof(double));

  if ((sumB < n_attributes) && (y < n_attributes)) {

    double B_j;

    for (int j = y + 1; j < n_attributes; j++) {

      B_j = Bv[j];

      if (B_j == 1)
        continue;

      for (int g_idx = 0; g_idx < n_grades; g_idx++) {

        double g = grades_set[g_idx];

        if (verbose) {

          Rcout << "Probando j = " << j << " y g = " << g << std::endl;
          Rcout << "  B_j = " << B_j << std::endl;

        }

        // conditions to avoid this step:
        if (B_j >= g) {

          // We move forward several steps
          while (g <= B_j) {

            g_idx++;
            if (g_idx >= n_grades) break;
            g = grades_set[g_idx];

          }

          // Es importante también comprobarlo después del bucle
          if (g_idx >= n_grades) continue;

          // continue;

        }

        if (verbose) {

          Rcout << "    Superado el primer test" << std::endl;
        }

        // First partial canonicity test
        if (j > 0) {

          int s = 0;
          for (s = 0; s < j; s++) {

            if (Ny[g_idx * n_attributes * n_attributes + j * n_attributes + s] > Bv[s])
              break;

          }

          if (s < j) {

            continue;

          }

        }

        if (verbose) {

          Rcout << "    Superado el segundo test" << std::endl;
        }

        // Obtain next concept
        reinitVector(&foo);
        cloneVector(&foo, A);

        bool is_parent = true;
        // Intersect A with {g/j}^{\downarrow}
        // foo is "C" in the pseudocode
        double sumC = 0.0;
        for (int id = 0; id < foo.i.used; id++) {

          int i = foo.i.array[id];
          double val = (I[j * n_objects + i] >= g) ? 1.0 : I[j * n_objects + i];

          if (val > foo.x.array[id]) {

            val = foo.x.array[id];

          }

          // This allows to check if C = A
          if (val < foo.x.array[id])
            is_parent = false;

          foo.x.array[id] = val;
          sumC += val;

        }

        if (verbose) {

          Rcout << "    - Studying extent: ";

          int count = 0;
          for (int z = 0; z < n_objects; z++) {

            if ((count < foo.i.used) && (z == foo.i.array[count])) {

              Rcout << foo.x.array[count] << " ";
              count++;

            } else {

              Rcout << "0 ";

            }

          }
          Rcout << std::endl;

        }

        reinitVector(&D);
        compute_intent(&D, foo, I, n_objects, n_attributes, tnorm, implication);
        (*intent_count) += n_attributes;
        (*closure_count) += 1.0;

        // gv is C^{\uparrow}\cap m_j
        gv = get_element(D, j);
        // gv = 1;
        // for (int i = 0; i < foo.i.used; i++) {
        //
        //   int n = foo.i.array[i];
        //   double val = (foo.x.array[i] <= I[j * n_objects + n]) ? 1.0 : I[j * n_objects + n];
        //
        //   if (val < gv) gv = val;
        //
        // }
        // (*partial_canonicity_tests) += 1.0;
        // (*all_att_intents) += 1.0;

        // If C^{\uparrow}\cap m_j != g/m_j, next value of grade
        if (gv != g) {

          // We move forward several steps
          while (g < gv) {

            g_idx++;
            if (g_idx >= n_grades) break;
            g = grades_set[g_idx];

          }
          if (g_idx >= n_grades) continue;

          if (verbose) {

            Rcout << "     Moving to g = " << g << std::endl;

          }

        }

        // Determine canonicity
        bool canonical = true;

        if (j > 0) {

          int s = 0;

          for (int is = 0; is < D.i.used; is++) {

            int iD = D.i.array[is];

            if (iD >= j)
              break;

            while (s < iD) {

              if (Bv[s] > 0) {

                canonical = false;
                break;

              }
              s++;

            }

            if (!canonical) break;

            if (s == iD) {

              if (Bv[s] < D.x.array[is]) {

                canonical = false;
                break;

              }
              s++;

            }

          }

        }

        if (canonical) {

          if (verbose) {

            Rcout << "     Added extent " << ": ";

            int count = 0;
            for (int z = 0; z < n_objects; z++) {

              if ((count < foo.i.used) && (z == foo.i.array[count])) {

                Rcout << foo.x.array[count] << " ";
                count++;

              } else {

                Rcout << "0 ";

              }

            }
            Rcout << std::endl;

            Rcout << "     With initial intent: ";
            count = 0;
            for (int z = 0; z < n_attributes; z++) {

              if ((count < D.i.used) && (z == D.i.array[count])) {

                Rcout << D.x.array[count] << " ";
                count++;

              } else {

                Rcout << "0 ";

              }

            }
            Rcout << std::endl;

          }

          add_column(&queue_A, foo);
          add_column(&queue_B, D);
          // insertArray(&queue_gr, g_idx);
          insertArray(&queue_y, j);

        } else {

          for (int idx = 0; idx < D.i.used; idx++) {

            int linear_index = g_idx * n_attributes * n_attributes + j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];

          }

          if (verbose)
            Rcout << "     Rejected extent, updating Mj" << std::endl;

        }

      }

    }

  }

  for (int cols = 0; cols < queue_y.used; cols++) {

    FuzzyFastGenerateFrom4(I,
                           // myI,
                           n_objects,
                           n_attributes,
                           n_grades,
                           attrs,
                           tnorm,
                           implication,
                           grades_set,
                           extents,
                           intents,
                           queue_A,
                           queue_B,
                           cols,
                           queue_y.array[cols],
                                        // queue_gr.array[cols],
                                                      Mj,
                                                      closure_count,
                                                      intent_count,
                                                      total,
                                                      depth + 1,
                                                      verbose);


  }

  freeVector(&queue_A);
  freeVector(&queue_B);
  freeArray(&queue_y);
  // freeArray(&queue_gr);
  freeVector(&foo);
  freeVector(&D);

  free(Mj);
  Mj = NULL;

  free(Bv);
  Bv = NULL;

  freeVector(&A);
  freeVector(&B);

}

// [[Rcpp::export]]
List FuzzyFCbO(NumericMatrix I,
               NumericVector grades_set,
               StringVector attrs,
               String connection = "standard",
               String name = "Zadeh",
               bool verbose = false) {

  // Rcout << "Start" << std::endl;

  LogicOperator implication = get_implication(name);
  LogicOperator tnorm = get_tnorm(name);
  GaloisOperator intent_f = get_intent_function(connection);
  GaloisOperator extent_f = get_extent_function(connection);


  Timer timer;
  timer.step("start");

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  double closure_count = 0;
  double intent_count = 0;
  double total = 0;
  double memory = 0;



  // Rcout << "A1" << std::endl;

  SparseVector B; // Empty intent
  initVector(&B, n_attributes);

  SparseVector A;
  initVector(&A, n_objects);
  compute_extent(&A, B, I.begin(), n_objects, n_attributes, tnorm, implication);
  freeVector(&B);

  SparseVector C;
  initVector(&C, n_attributes);
  compute_intent(&C, A, I.begin(), n_objects, n_attributes, tnorm, implication);
  intent_count += n_attributes;

  // total++;

  SparseVector intents;
  SparseVector extents;
  initVector(&intents, n_attributes);
  initVector(&extents, n_objects);

  // Rcout << "A2" << std::endl;

  double* Ny = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  // memory = memory + 8 * n_attributes * n_attributes * n_grades  / 1000;

  // Rcout << "Initial allocation: " << memory << " Kb." << std::endl;

  // Rcout << memory << std::endl;

  FuzzyFastGenerateFrom4(I.begin(),
                         // myI,
                         n_objects,
                         n_attributes,
                         n_grades,
                         attrs,
                         tnorm, implication,
                         grades_set.begin(),
                         &extents,
                         &intents,
                         A,
                         C,
                         0,
                         -1,
                         // n_grades + 1,
                         Ny,
                         &closure_count,
                         &intent_count,
                         &total,
                         0,
                         verbose);

  free(Ny);

  // Rcout << "A3" << std::endl;


  // memory = memory - 8 * n_attributes * n_attributes * n_grades / 1000;

  // Rcout << "Freed from initial allocation: " << memory << " Kb." << std::endl;
  //
  // Rcout << " Number of closures: " << closure_count << std::endl;
  // Rcout << " Number of intent computations: " << intent_count << std::endl;

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&extents);

  timer.step("end");

  // Rcout << "A4" << std::endl;


  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = total,
    _["closure_count"] = closure_count,
    _["intent_count"] = intent_count,
    _["timer"] = timer);

  return res;

}


