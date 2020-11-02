#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;

void FuzzyFastGenerateFrom(NumericMatrix I,
                           StringVector attrs,
                           NumericVector grades_set,
                           SparseVector *extents,
                           SparseVector *intents,
                           SparseVector A, // extent
                           SparseVector B, //intent
                           int y,
                           int gr,
                           const NumericVector Ny,
                           int* closure_count,
                           int depth = 0) {

  // Rcout << "DEPTH = " << depth << std::endl;
  // print_matrix(Ny);

  // int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();


  // Add <A, B> to list of concepts.
  add_column(extents, A);
  add_column(intents, B);

  if ((cardinal(B) == n_attributes) || (y >= n_attributes)) return;

  // Prepare to queue next recursion step.
  std::list<SparseVector> queue_A, queue_B;
  std::list<int> queue_y, queue_gr;

  NumericVector Mj(Rcpp::clone(Ny));

  IntegerVector dims = Ny.attr("dim");

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

          // Rcout << "Falla condición 1" << std::endl;
          break;

        }

        if ((j == y) && (g_idx >= gr)) {

          // Rcout << "Falla condición 2" << std::endl;
          continue;

        }

        if (j > 0) {

          int s = 0;
          for (s = 0; s < j; s++) {

            if (get_element_array(Ny, s, j, g_idx) > get_element(B, s))
              break;

          }

          if (s < j) {

            // Rcout << "Falla condición 3:" << std::endl;
            //
            // Rcout << "   Ny(" << s << ", " << j << ") = " << Ny(s, j) << std::endl;
            // Rcout << "   B(" << s << ") = " << get_element(B, s) << std::endl;

            continue;

          }

        }

        // Obtain next concept
        NumericVector foo = zadeh_I(grades_set[g_idx], I(_, j));
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

        SparseVector C2 = as_sparse(C);
        // SparseVector C2 = as_sparse(foo);
        SparseVector D = compute_intent(C2, I);

        (*closure_count)++;

        // Rcout << "Added ";
        // printVector(D, attrs);
        // Rcout << std::endl;

        bool canonical = true;
        if (j > 0) {

          int s = 0;

          for (s = 0; s < j; s++) {

            // Rcout << "B(" << s << ") = " << get_element(B, s) << std::endl;
            // Rcout << "D(" << s << ") = " << get_element(D, s) << std::endl;

            if (get_element(B, s) < get_element(D, s)) {

              canonical = false;
              break;

            }

          }

          // if (s < D.length)
          //   canonical = false;

        }

        if (canonical) {

          // Probably we could use this knowledge to
          // accelerate the algorithm.
          if (get_element(D, j) > grades_set[g_idx])
            continue;

          queue_A.push_back(C2);
          queue_B.push_back(D);
          queue_gr.push_back(g_idx);
          queue_y.push_back(j);

        } else {

          // Rcout << "Not canonical" << std::endl;

          for (int idx = 0; idx < D.i.used; idx++) {

            int linear_index = g_idx * dims[0] * dims[1] + j * dims[0] + D.i.array[idx];

            Mj[linear_index] = D.x.array[idx];

          }

        }

      }

    }

  }

  // Rcout << "Queue length: " << queue_y.size() << std::endl;

  while (queue_A.size() > 0) {

    FuzzyFastGenerateFrom(I,
                          attrs,
                          grades_set,
                          extents,
                          intents,
                          queue_A.front(),
                          queue_B.front(),
                          queue_y.front(),
                          queue_gr.front(),
                          Mj,
                          closure_count,
                          depth + 1);

    freeVector(&(queue_A.front()));
    freeVector(&(queue_B.front()));

    queue_A.pop_front();
    queue_B.pop_front();
    queue_y.pop_front();
    queue_gr.pop_front();

  }


}

// [[Rcpp::export]]
List FuzzyFastCbo_C(NumericMatrix I,
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

  NumericVector Ny(Dimension(n_attributes, n_attributes, n_grades));
  // Ny.fill(1);

  FuzzyFastGenerateFrom(I,
                        attrs,
                        grades_set,
                        &extents,
                        &intents,
                        A,
                        C,
                        0,
                        n_grades + 1,
                        Ny,
                        &closure_count,
                        0);

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

// [[Rcpp::export]]
IntegerVector my_dims(NumericVector I) {

  // IntegerVector res(2);
  // res[0] = I.size();
  // res[1] = 1;

  IntegerVector res(I.attr("dim"));

  return res;
}



// [[Rcpp::export]]
void write_element_array(NumericVector I,
                         int i, int j, int k,
                         double x) {

  IntegerVector res(I.attr("dim"));
  // NumericVector A(I);

  int linear_index = k * res[0] * res[1] + j * res[0] + i;

  I.begin()[linear_index] = x;

}
