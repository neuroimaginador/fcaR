#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;

#define MAX_CONCEPTS 4000000

bool is_canonical(double* I,
                  double* A,
                  double* B,
                  int n_objects,
                  int n_attributes,
                  int r, int rnew,
                  int j,
                  int gr) {

  bool res = true;

  if (j > 0) {

    for (int k = 0; k < j; k++) {

      double gv = 1;
      for (int i = 0; i < n_objects; i++) {

        double val = (A[rnew * n_objects + i] <= I[k * n_objects + i]) ? 1.0 : I[k * n_objects + i];

        // if (A[rnew * n_objects + i] <= I[k * n_objects + i]) {
        //
        //   val = 1;
        //
        // } else {
        //
        //   val = I[k * n_objects + i];
        //
        // }

        if (val < gv) gv = val;

        if (gv == 0) break;

      }

      if (gv > B[r * n_attributes + k]) {

        return false;
        break;

      }

    }

  }

  return res;

}

bool is_canonical(double* I,
                  SparseVector A,
                  double* B,
                  int n_objects,
                  int n_attributes,
                  int r, int rnew,
                  int j,
                  int gr) {

  bool res = true;

  if (j > 0) {

    for (int k = 0; k < j; k++) {

      double gv = 1;
      for (int i = 0; i < A.i.used; i++) {

        int n = A.i.array[i];

        double val = (A.x.array[i] <= I[k * n_objects + n]) ? 1.0 : I[k * n_objects + n];

        if (val < gv) gv = val;

        if (gv == 0) break;

      }

      if (gv > B[r * n_attributes + k]) {

        return false;
        break;

      }

    }

  }

  return res;

}

void inclose_core(double* I,
                  double* grades_set,
                  int r,
                  int y,
                  int gr,
                  int n_objects,
                  int n_attributes,
                  int n_grades,
                  double* A,
                  double* B,
                  int* rnew,
                  int* intent_count,
                  int depth) {

  // if ((*rnew) > 900000) return;
  //
  // Rcout << "DEPTH = " << depth << std::endl;
  // Rcout << "parent = " << r << std::endl;
  // Rcout << "rnew = " << *rnew << std::endl;
  //
  // Rcout << "y = " << y << std::endl;
  // Rcout << "gr = " << gr << std::endl;

  if (y > n_attributes - 1) return;

  IntArray gchildren, jchildren, rchildren;
  initArray(&gchildren, n_attributes * n_grades);
  initArray(&jchildren, n_attributes * n_grades);
  initArray(&rchildren, n_attributes * n_grades);

  double g, gv;
  SparseVector Ar, Arnew;
  initVector(&Ar, n_objects);
  initVector(&Arnew, n_objects);

  // int j = y;

  for (int j = y; j < n_attributes; j++) {

    for (int g_idx = n_grades - 1; g_idx >= 0; g_idx--) {

      if ((j == y) && (g_idx > gr)) continue;

      // Rcout << "Probando j = " << j << " y g_idx = " << g_idx << std::endl;

      g = grades_set[g_idx];

      if (B[r * n_attributes + j] < 1) {

        reinitVector(&Arnew);
        reinitVector(&Ar);
        // SparseVector Arnew;
        // initVector(&Arnew, n_objects);

        for (int i = 0; i < n_objects; i++) {

          if (A[r * n_objects + i] > 0) {

            insertArray(&(Arnew.i), i);
            insertArray(&(Arnew.x), A[r * n_objects + i]);

          }

        }

        insertArray(&(Arnew.p), 0);
        insertArray(&(Arnew.p), Arnew.i.used);

        // SparseVector Ar;
        // initVector(&Ar, n_objects);

        // cloneVector(&Ar, Arnew);

        bool is_parent = true;

        // Intersect A with {g/j}
        for (int id = 0; id < Arnew.i.used; id++) {

          int i = Arnew.i.array[id];
          double val = (I[j * n_objects + i] >= g) ? 1.0 : I[j * n_objects + i];

          if (val > Arnew.x.array[id]) {

            val = Arnew.x.array[id];

          }

          if (val < Arnew.x.array[id])
            is_parent = false;

          Arnew.x.array[id] = val;

        }
        // for (int i = 0; i < n_objects; i++) {
        //
        //   double val = (I[j * n_objects + i] >= g) ? 1.0 : I[j * n_objects + i];
        //
        //   if (A[r * n_objects + i] < val) {
        //
        //     val = A[r * n_objects + i];
        //
        //   }
        //
        //   A[(*rnew) * n_objects + i] = val;
        //
        // }

        // Rcout << "The parent is:" << std::endl;
        // for (int h = 0; h < n_objects; h++) {
        //
        //   Rcout << A[r * n_objects + h] << " ";
        //
        // }
        // Rcout << std::endl;
        //
        // Rcout << "The new candidate is:" << std::endl;
        // for (int h = 0; h < n_objects; h++) {
        //
        //   Rcout << A[(*rnew) * n_objects + h] << " ";
        //
        // }
        // Rcout << std::endl;

        // printArray(Arnew.i);
        // printArray(Arnew.x);

        gv = 1;
        for (int i = 0; i < Arnew.i.used; i++) {

          int n = Arnew.i.array[i];
          double val = (Arnew.x.array[i] <= I[j * n_objects + n]) ? 1.0 : I[j * n_objects + n];

          if (val < gv) gv = val;

        }

        (*intent_count)++;
        // for (int i = 0; i < n_objects; i++) {
        //
        //   double val = (A[(*rnew) * n_objects + i] <= I[j * n_objects + i]) ? 1.0 : I[j * n_objects + i];
        //
        //   if (val < gv) gv = val;
        //
        // }

        // Rcout << "g is " << g << " and gv is " << gv << std::endl;

        // bool is_parent = true;
        //
        // for (int i = 0; i < n_objects; i++) {
        //
        //   if (A[r * n_objects + i] != A[(*rnew) * n_objects + i]) {
        //
        //     is_parent = false;
        //     break;
        //
        //   }
        //
        // }

        if (is_parent) {

          // Rcout << "The candidate has been rejected: equal to parent." << std::endl;
          // Rcout << "Changing intent of parent = " << r << std::endl;
          // Rcout << "For j = " << j << " we had g = " << B[r * n_attributes + j] << "and now it is gv = " << gv << std::endl;

          B[r * n_attributes + j] = gv;

        } else {

          if (gv > g) continue;

          if (is_canonical(I, Arnew, B, n_objects, n_attributes, r, (*rnew), j, g_idx)) {

            // Rcout << "Added extent " << (*rnew) << ": ";
            //
            // for (int z = 0; z < n_objects; z++) {
            //
            //   Rcout << A[(*rnew) * n_objects + z] << " ";
            //
            // }
            // Rcout << std::endl;

            insertArray(&jchildren, j);
            insertArray(&gchildren, g_idx);
            insertArray(&rchildren, (*rnew));

            for (int i = 0; i < n_attributes; i++) {

              B[(*rnew) * n_attributes + i] = B[r * n_attributes + i];

            }

            B[(*rnew) * n_attributes + j] = gv;

            for (int i = 0; i < Arnew.i.used; i++) {

              A[(*rnew) * n_objects + Arnew.i.array[i]] = Arnew.x.array[i];

            }

            // Rcout << "With initial intent: ";
            //
            // for (int z = 0; z < n_attributes; z++) {
            //
            //   Rcout << B[(*rnew) * n_attributes + z] << " ";
            //
            // }
            // Rcout << std::endl;

            (*rnew)++;

            // Rcout << (*rnew) << std::endl;

          } else {

            // Rcout << "The candidate has been rejected: no canonical" << std::endl;

          }

        }

        // freeVector(&Arnew);
        // freeVector(&Ar);

      }

    }

  }

  freeVector(&Ar);
  freeVector(&Arnew);


  // Loop over children
  for (int i = 0; i < jchildren.used; i++) {

    inclose_core(I,
                 grades_set,
                 rchildren.array[i],
                 jchildren.array[i],
                 gchildren.array[i],
                 n_objects,
                 n_attributes,
                 n_grades,
                 A,
                 B,
                 rnew,
                 intent_count,
                 depth + 1);

  }

  freeArray(&gchildren);
  freeArray(&jchildren);
  freeArray(&rchildren);

}

// [[Rcpp::export]]
int inclose_C(NumericMatrix I,
              NumericVector grades_set) {

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  int intent_count = 0;

  double* A = (double*)calloc(n_objects * MAX_CONCEPTS, sizeof(double*));
  double* B = (double*)calloc(n_attributes * MAX_CONCEPTS, sizeof(double*));

  // First extent
  for (int obj = 0; obj < n_objects; obj++) {

    A[obj] = 1;

  }

  int rnew = 1;

  inclose_core(I.begin(),
               grades_set.begin(),
               0,
               0,
               n_grades,
               n_objects,
               n_attributes,
               n_grades,
               A,
               B,
               &rnew,
               &intent_count,
               0);

  Rcout << " Number of intent computations: " << intent_count << std::endl;


  free(A);
  free(B);

  return rnew;

}
