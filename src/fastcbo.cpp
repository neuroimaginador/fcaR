#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;


void FuzzyFastGenerateFrom4(double* I,
                            const SparseVector myI,
                            int n_objects,
                            int n_attributes,
                            int n_grades,
                            StringVector attrs,
                            double* grades_set,
                            SparseVector *extents,
                            SparseVector *intents,
                            SparseVector qA, // extent
                            SparseVector qB, //intent
                            int id_col,
                            int y,
                            int gr,
                            double* Ny,
                            int* closure_count,
                            int* intent_count,
                            int depth = 0) {

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

  if ((cardinal(B) == n_attributes) || (y >= n_attributes)) return;

  double* Bv = (double*)calloc(B.length, sizeof(double));
  for (int i = 0; i < B.i.used; i++) {

    Bv[B.i.array[i]] = B.x.array[i];

  }

  // double* D1 = (double*)calloc(n_attributes, sizeof(double));

  // Prepare to queue next recursion step.
  SparseVector queue_A, queue_B;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);

  IntArray queue_y, queue_gr;
  initArray(&queue_y, n_attributes * n_grades);
  initArray(&queue_gr, n_attributes * n_grades);

  double* Mj = (double*)malloc(n_attributes * n_attributes * n_grades * sizeof(double));

  memcpy(Mj, Ny, n_attributes * n_attributes * n_grades * sizeof(double));

  if ((cardinal(B) < n_attributes) && (y < n_attributes)) {

    double B_j;

    SparseVector foo, D;
    initVector(&foo, n_objects);
    initVector(&D, n_attributes);

    for (int j = y; j < n_attributes; j++) {

      // Rcout << "Probando j = " << j << std::endl;

      B_j = Bv[j];

      if (B_j == 1)
        continue;

      for (int g_idx = n_grades - 1; g_idx >= 0; g_idx--) {

        // conditions to avoid this step:
        if (B_j >= grades_set[g_idx]) {

          break;

        }

        if ((j == y) && (g_idx >= gr)) {

          continue;

        }

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

        // Obtain next concept
        reinitVector(&foo);
        get_column(&foo, myI, g_idx * n_attributes + j);

        intersect(&foo, A);

        reinitVector(&D);
        compute_intent(&D, foo, I, n_objects, n_attributes);
        (*intent_count) += n_attributes;

        // for (int h = 0; h < D.i.used; h++) {
        //
        //   D1[D.i.array[h]] = D.x.array[h];
        //
        // }

        (*closure_count)++;

        bool canonical = true;
        // for (int is = 0; is < j; is++) {
        //
        //   if (Bv[is] < D1[is]) {
        //
        //     canonical = false;
        //     break;
        //
        //   }
        //
        // }

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

          // Probably we could use this knowledge to
          // accelerate the algorithm.
          if (get_element(D, j) > grades_set[g_idx])
          // if (D1[j] > grades_set[g_idx])
            continue;

          add_column(&queue_A, foo);
          add_column(&queue_B, D);
          insertArray(&queue_gr, g_idx);
          insertArray(&queue_y, j);

        } else {

          for (int idx = 0; idx < D.i.used; idx++) {

            int linear_index = g_idx * n_attributes * n_attributes + j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];

          }

        }

        // for (int h = 0; h < D.i.used; h++) {
        //
        //   D1[D.i.array[h]] = 0;
        //
        // }
      }

    }

    freeVector(&foo);
    freeVector(&D);

  }

  for (int cols = 0; cols < queue_y.used; cols++) {

    FuzzyFastGenerateFrom4(I,
                           myI,
                           n_objects,
                           n_attributes,
                           n_grades,
                           attrs,
                           grades_set,
                           extents,
                           intents,
                           queue_A,
                           queue_B,
                           cols,
                           queue_y.array[cols],
                           queue_gr.array[cols],
                           Mj,
                           closure_count,
                           intent_count,
                           depth + 1);


  }

  freeVector(&queue_A);
  freeVector(&queue_B);
  freeArray(&queue_y);
  freeArray(&queue_gr);

  free(Mj);
  Mj = NULL;

  free(Bv);
  Bv = NULL;

  // free(D1);
  // D1 = NULL;

  freeVector(&A);
  freeVector(&B);

}

// [[Rcpp::export]]
List FuzzyFastCbO(NumericMatrix I,
                  StringVector attrs,
                  NumericVector grades_set) {

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  int closure_count = 0;
  int intent_count = 0;
  // double memory = 0;

  SparseVector myI;
  initMatrix(&myI, n_objects);
  SparseVector I1;
  initVector(&I1, n_objects);

  for (int g = 0; g < n_grades; g++) {

    for (int i = 0; i < n_attributes; i++) {

      reinitVector(&I1);

      as_sparse(&I1, I.begin(), n_objects, n_attributes, i);

      zadeh_I(grades_set[g], &I1);
      add_column(&myI, I1);
      // freeVector(&I1);

    }

  }

  freeVector(&I1);

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

  closure_count++;

  SparseVector intents;
  SparseVector extents;
  initVector(&intents, n_attributes);
  initVector(&extents, n_objects);

  double* Ny = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  // memory = memory + 8 * n_attributes * n_attributes * n_grades  / 1000;

  // Rcout << "Initial allocation: " << memory << " Kb." << std::endl;

  // Rcout << memory << std::endl;

  FuzzyFastGenerateFrom4(I.begin(),
                         myI,
                         n_objects,
                         n_attributes,
                         n_grades,
                         attrs,
                         grades_set.begin(),
                         &extents,
                         &intents,
                         A,
                         C,
                         0,
                         0,
                         n_grades + 1,
                         Ny,
                         &closure_count,
                         &intent_count,
                         0);

  free(Ny);

  // memory = memory - 8 * n_attributes * n_attributes * n_grades / 1000;

  // Rcout << "Freed from initial allocation: " << memory << " Kb." << std::endl;



  // Rcout << " Number of closures: " << closure_count << std::endl;
  // Rcout << " Number of intent computations: " << intent_count << std::endl;

  // S4 intents_S4 = SparseToS4_fast(intents);
  // S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&extents);

  List res = List::create(
    // _["intents"] = intents_S4,
                          // _["extents"] = extents_S4,
                          _["closure_count"] = closure_count,
                          _["intent_count"] = (double)intent_count / (double)(n_attributes * n_grades));

  return res;

}


