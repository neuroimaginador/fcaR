#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;


void FuzzyFastGenerateFrom3(NumericMatrix I,
                            int n_attributes,
                            int n_grades,
                            StringVector attrs,
                            NumericVector grades_set,
                            SparseVector *extents,
                            SparseVector *intents,
                            SparseVector qA, // extent
                            SparseVector qB, //intent
                            int id_col,
                            int y,
                            int gr,
                            const double* Ny,
                            int* closure_count,
                            int depth = 0) {

  // Rcout << "DEPTH = " << depth << std::endl;
  // Rcout << "y = " << y << std::endl;


  // A y B pasan a ser matrices (se forman con
  // add_column) en lugar de con std::list push_back
  // Luego queue_A y queue_B serán sparsevector
  // queue_y y queue_gr IntegerArray o similar.
  // Habrá que pasar el índice de la columna de
  // queue_A y queue_B que hace el papel de A y de B
  // al llamar al proceso hijo
  // Así solo hay que "limpiar" la memoria una vez
  // tras haber hecho el bucle que llama a los procesos
  // hijos (for int i = 0; i < queue_A.p.i.used; i++)
  // get_element_array probablemente se pueda
  // eliminar.
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

  // Prepare to queue next recursion step.
  SparseVector queue_A, queue_B;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);

  IntArray queue_y, queue_gr;
  initArray(&queue_y, 10);
  initArray(&queue_gr, 10);

  double* Mj = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  memcpy(Mj, Ny, n_attributes * n_attributes * n_grades * sizeof(double));


  if ((cardinal(B) < n_attributes) && (y < n_attributes)) {

    double B_j;

    for (int j = y; j < n_attributes; j++) {

      // Rcout << "Probando j = " << j << std::endl;

      B_j = get_element(B, j);

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

            if (Ny[g_idx * n_attributes * n_attributes + j * n_attributes + s] > get_element(B, s))
              break;

          }

          if (s < j) {

            continue;

          }

        }

        // Obtain next concept
        NumericVector foo = zadeh_I(grades_set[g_idx], I(_, j));

        // Rcout << "Column " << j << " and grade = " << grades_set[g_idx] << std::endl;
        // SparseVector foo2 = as_sparse(foo);
        // printArray(foo2.i);
        // printArray(foo2.x);
        // freeVector(&foo2);
        // print_vector(foo, foo.size());

        //
        //         for (int k = 0; k < A.i.used; k++) {
        //
        //           if (foo[A.i.array[k]] > A.x.array[k])
        //             foo[A.i.array[k]] = A.x.array[k];
        //
        //         }


        NumericVector C = as_vector(A);

        for (int s = 0; s < C.size(); s++) {

          if (foo[s] < C[s]) {

            C[s] = foo[s];

          }

        }

        // Rcout << "A is:" << std::endl;
        // printArray(A.i);
        // printArray(A.x);


        SparseVector C2 = as_sparse(C);
        // Rcout << "After intersection:" << std::endl;
        // printArray(C2.i);
        // printArray(C2.x);


        SparseVector D = compute_intent(C2, I);

        (*closure_count)++;

        // Rcout << "Added ";
        // printVector(D, attrs);
        // Rcout << std::endl;

        bool canonical = true;
        if (j > 0) {

          int s = 0;

          for (s = 0; s < j; s++) {

            if (get_element(B, s) < get_element(D, s)) {

              canonical = false;
              break;

            }

          }

        }

        if (canonical) {

          // Probably we could use this knowledge to
          // accelerate the algorithm.
          if (get_element(D, j) > grades_set[g_idx])
            continue;

          add_column(&queue_A, C2);
          add_column(&queue_B, D);
          insertArray(&queue_gr, g_idx);
          insertArray(&queue_y, j);

        } else {

          for (int idx = 0; idx < D.i.used; idx++) {

            int linear_index = g_idx * n_attributes * n_attributes + j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];

          }

        }

      }

    }

  }

  for (int cols = 0; cols < queue_y.used; cols++) {

    FuzzyFastGenerateFrom3(I,
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
                           depth + 1);


  }

  freeVector(&queue_A);
  freeVector(&queue_B);
  freeArray(&queue_y);
  freeArray(&queue_gr);
  //
  free(Mj);

  freeVector(&A);
  freeVector(&B);


}

// [[Rcpp::export]]
List FuzzyFastCbo_C3(NumericMatrix I,
                     StringVector attrs,
                     NumericVector grades_set) {

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  int closure_count = 0;

  SparseVector B; // Empty intent
  initVector(&B, n_attributes);

  SparseVector A;
  A = compute_extent(B, I);
  freeVector(&B);

  SparseVector C = compute_intent(A, I);

  closure_count++;

  SparseVector intents;
  SparseVector extents;
  initVector(&intents, n_attributes);
  initVector(&extents, n_objects);

  double* Ny = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  FuzzyFastGenerateFrom3(I,
                         n_attributes,
                         n_grades,
                         attrs,
                         grades_set,
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

  Rcout << " Number of closures: " << closure_count << std::endl;

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&extents);

  List res = List::create(_["intents"] = intents_S4,
                          _["extents"] = extents_S4,
                          _["closure_count"] = closure_count);

  return res;

}

//[[Rcpp::export]]
void test_SparseVector() {

  SparseVector A, B;

  initVector(&A, 10);
  initVector(&B, 10);

  Rcout << "Inicializado A" << std::endl;
  Rcout << "Usados p = " << A.p.used << std::endl;

  insertArray(&(A.i), 5);
  insertArray(&(A.x), 0.1);

  Rcout << "Añadido A" << std::endl;
  Rcout << "Usados p = " << A.p.used << std::endl;
  Rcout << "Usados i = " << A.i.used << std::endl;

  insertArray(&(B.i), 3);
  insertArray(&(B.x), 0.5);

  Rcout << "Añadido B" << std::endl;
  Rcout << "Usados p = " << B.p.used << std::endl;
  Rcout << "Usados i = " << B.i.used << std::endl;

  add_column(&A, B);

  Rcout << "Añadido B a A" << std::endl;
  Rcout << "Usados p = " << A.p.used << std::endl;
  Rcout << "Usados i = " << A.i.used << std::endl;

  Rcout << "p = " << A.p.array[0] << ", " << A.p.array[1] << std::endl;
  Rcout << "i = " << A.i.array[0] << ", " << A.i.array[1] << std::endl;



  freeVector(&A);
  freeVector(&B);

}

