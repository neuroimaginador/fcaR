#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;


void FuzzyFastGenerateFrom5(double* I,
                            const SparseVector myI,
                            int n_objects,
                            int n_attributes,
                            int n_grades,
                            StringVector attrs,
                            double* grades_set,
                            SparseVector *extents,
                            SparseVector *intents,
                            SparseVector *qA, // extent
                            SparseVector *qB, //intent
                            int id_col,
                            int y,
                            int gr,
                            double* Ny,
                            int* closure_count,
                            int depth = 0) {

  // Rcout << "DEPTH = " << depth << std::endl;
  // Rcout << "y = " << y << std::endl;

  // Se puede pasar la propia I a una lista de SparseVector
  // o el hacer el zadeh_I(g, I[, j]) en esa lista, para
  // evitar duplicar los cálculos


  SparseVector *A, *B;
  A = (SparseVector*)calloc(1, sizeof(SparseVector));
  B = (SparseVector*)calloc(1, sizeof(SparseVector));
  initVector(A, qA->length);
  initVector(B, qB->length);
  get_column(A, *qA, id_col);
  get_column(B, *qB, id_col);

  double* Bv = (double*)calloc(B->length, sizeof(double));

  for (int i = 0; i < B->length; i++) {

    Bv[i] = get_element(*B, i);

  }


  // Add <A, B> to list of concepts.
  add_column(extents, *A);
  add_column(intents, *B);

  if ((cardinal(*B) == n_attributes) || (y >= n_attributes)) return;

  // Prepare to queue next recursion step.
  SparseVector *queue_A, *queue_B;
  queue_A = (SparseVector*)calloc(1, sizeof(SparseVector));
  queue_B = (SparseVector*)calloc(1, sizeof(SparseVector));

  initVector(queue_A, qA->length);
  initVector(queue_B, qB->length);

  IntArray *queue_y, *queue_gr;
  queue_y = (IntArray*)calloc(1, sizeof(IntArray));
  queue_gr = (IntArray*)calloc(1, sizeof(IntArray));
  initArray(queue_y, n_attributes * n_grades);
  initArray(queue_gr, n_attributes * n_grades);

  double* Mj = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  // (*memory) += 8 * n_attributes * n_attributes * n_grades / 1000;
  // Rcout << "Allocation at depth " << depth << ": " << (*memory) << " Kb." << std::endl;

  memcpy(Mj, Ny, n_attributes * n_attributes * n_grades * sizeof(double));


  if ((cardinal(*B) < n_attributes) && (y < n_attributes)) {

    double B_j;

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
        SparseVector *foo;
        foo = (SparseVector*)calloc(1, sizeof(SparseVector));
        initVector(foo, n_objects);
        get_column(foo, myI, g_idx * n_attributes + j);

        intersect(foo, *A);

        SparseVector *D;
        D = (SparseVector*)calloc(1, sizeof(SparseVector));
        initVector(D, n_attributes);
        compute_intent(D, *foo, I, n_objects, n_attributes);

        (*closure_count)++;

        bool canonical = true;
        if (j > 0) {

          int s = 0;

          for (int is = 0; is < D->i.used; is++) {

            int iD = D->i.array[is];

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

              if (Bv[s] < D->x.array[is]) {

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
          if (get_element(*D, j) > grades_set[g_idx])
            continue;

          add_column(queue_A, *foo);
          add_column(queue_B, *D);
          insertArray(queue_gr, g_idx);
          insertArray(queue_y, j);

        } else {

          for (int idx = 0; idx < D->i.used; idx++) {

            int linear_index = g_idx * n_attributes * n_attributes + j * n_attributes + D->i.array[idx];
            Mj[linear_index] = D->x.array[idx];

          }

        }

        freeVector(D);
        free(D);
        freeVector(foo);
        free(foo);

      }

    }

  }

  // Rcout << "Queue size = " << queue_y.used << std::endl;

  for (int cols = 0; cols < queue_y->used; cols++) {

    FuzzyFastGenerateFrom5(I,
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
                           queue_y->array[cols],
                           queue_gr->array[cols],
                           Mj,
                           closure_count,
                           depth + 1);


  }

  freeVector(queue_A);
  free(queue_A);
  freeVector(queue_B);
  free(queue_B);
  freeArray(queue_y);
  free(queue_y);
  freeArray(queue_gr);
  free(queue_gr);
  //
  free(Mj);
  Mj = NULL;

  free(Bv);
  Bv = NULL;

  // (*memory) -= 8 * n_attributes * n_attributes * n_grades  / 1000;

  // Rcout << "Freed from depth " << depth << ": " << (*memory) << " Kb." << std::endl;

  freeVector(A);
  free(A);
  freeVector(B);
  free(B);

}

// [[Rcpp::export]]
List FuzzyFastCbo_C5(NumericMatrix I,
                     StringVector attrs,
                     NumericVector grades_set) {

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  int closure_count = 0;
  // double memory = 0;
  //
  // Rcout << sizeof(SparseVector) << std::endl;

  SparseVector myI;
  initVector(&myI, n_objects);

  for (int g = 0; g < n_grades; g++) {

    for (int i = 0; i < n_attributes; i++) {

      SparseVector I1 = as_sparse(I.begin(), n_objects,
                                  n_attributes, i);

      zadeh_I(grades_set[g], &I1);
      add_column(&myI, I1);
      freeVector(&I1);

    }

  }

  SparseVector *B; // Empty intent
  B = (SparseVector*)calloc(1, sizeof(SparseVector));
  initVector(B, n_attributes);

  SparseVector *A;
  A = (SparseVector*)calloc(1, sizeof(SparseVector));
  initVector(A, n_objects);
  compute_extent(A, *B, I.begin(), n_objects, n_attributes);
  freeVector(B);

  SparseVector *C;
  C = (SparseVector*)calloc(1, sizeof(SparseVector));
  initVector(C, n_attributes);
  compute_intent(C, *A, I.begin(),
                      n_objects, n_attributes);

  closure_count++;

  SparseVector intents;
  SparseVector extents;
  initMatrix(&intents, n_attributes);
  initMatrix(&extents, n_objects);

  double* Ny = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  // memory = memory + 8 * n_attributes * n_attributes * n_grades  / 1000;

  // Rcout << "Initial allocation: " << memory << " Kb." << std::endl;

  FuzzyFastGenerateFrom5(I.begin(),
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
                         0);

  free(Ny);

  // memory = memory - 8 * n_attributes * n_attributes * n_grades / 1000;

  // Rcout << "Freed from initial allocation: " << memory << " Kb." << std::endl;



  // Rcout << " Number of closures: " << closure_count << std::endl;

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(A);
  free(A);
  freeVector(C);
  free(C);
  freeVector(&intents);
  freeVector(&extents);

  List res = List::create(_["intents"] = intents_S4,
                          _["extents"] = extents_S4,
                          _["closure_count"] = closure_count);

  return res;

}


