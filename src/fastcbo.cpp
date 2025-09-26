#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
#include "aux_functions.h"

using namespace Rcpp;


void FuzzyFastGenerateFrom4(double* I,
                            int n_objects,
                            int n_attributes,
                            int n_grades,
                            StringVector attrs,
                            double* grades_set,
                            SparseVector *extents,
                            SparseVector *intents,
                            const SparseVector& qA, // Usamos referencia para eficiencia
                            const SparseVector& qB, // Usamos referencia para eficiencia
                            int id_col,
                            int y,
                            const double* Ny, // Matriz del padre (solo lectura)
                            double* closure_count,
                            double* intent_count,
                            double* total,
                            int depth,
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

  double* Bv = (double*)calloc(B.length, sizeof(double));
  double sumB = 0.0;
  for (size_t i = 0; i < B.i.used; i++) {
    Bv[B.i.array[i]] = B.x.array[i];
    sumB += B.x.array[i];
  }

  // Se vuelve al sistema de colas robusto y correcto
  SparseVector queue_A, queue_B;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);
  IntArray queue_y;
  initArray(&queue_y, n_attributes * n_grades);

  SparseVector foo, D;
  initVector(&foo, n_objects);
  initVector(&D, n_attributes);

  // --- CORRECCIÓN CLAVE ---
  // Mj es una copia local de la matriz del padre (Ny).
  // Los cambios en Mj no afectarán a otras ramas.
  double* Mj = (double*)malloc((size_t)n_attributes * n_attributes * n_grades * sizeof(double));
  memcpy(Mj, Ny, (size_t)n_attributes * n_attributes * n_grades * sizeof(double));

  if ((sumB < n_attributes) && (y < n_attributes)) {
    for (int j = y + 1; j < n_attributes; j++) {
      double B_j = Bv[j];
      if (B_j == 1) continue;

      for (int g_idx = 0; g_idx < n_grades; g_idx++) {
        double g = grades_set[g_idx];

        if (B_j >= g) {
          // Avanzamos los grados para evitar cálculos redundantes
          while (g_idx < n_grades - 1 && g <= B_j) {
            g_idx++;
            g = grades_set[g_idx];
          }
          if (B_j >= g) continue;
        }

        // Primera prueba de canonicidad parcial
        if (j > 0) {
          int s = 0;
          for (s = 0; s < j; s++) {
            // Se usa Ny (la matriz del padre) para la comprobación
            if (Ny[(size_t)g_idx * n_attributes * n_attributes + (size_t)j * n_attributes + s] > Bv[s])
              break;
          }
          if (s < j) continue;
        }

        reinitVector(&foo);
        cloneVector(&foo, A);

        for (size_t id = 0; id < foo.i.used; id++) {
          int i = foo.i.array[id];
          double val = I[(size_t)j * n_objects + i];
          if(val < g){
            foo.x.array[id] = std::min(foo.x.array[id], val);
          }
        }

        reinitVector(&D);
        compute_intent(&D, foo, I, n_objects, n_attributes);
        (*intent_count) += n_attributes;
        (*closure_count) += 1.0;

        double gv = get_element(D, j);

        if (gv != g) {
          // Avanzamos los grados
          while (g_idx < n_grades - 1 && g < gv) {
            g_idx++;
            g = grades_set[g_idx];
          }
          if (g < gv) continue;
        }

        bool canonical = true;
        if (j > 0) {
          size_t s_idx = 0;
          for(size_t is = 0; is < D.i.used; ++is){
            int iD = D.i.array[is];
            if(iD >= j) break;
            if(Bv[iD] < D.x.array[is]){
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
          // Actualizamos nuestra copia local de Mj, no la del padre
          for (size_t idx = 0; idx < D.i.used; idx++) {
            size_t linear_index = (size_t)g_idx * n_attributes * n_attributes + (size_t)j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];
          }
        }
      }
    }
  }

  for (size_t cols = 0; cols < queue_y.used; cols++) {
    FuzzyFastGenerateFrom4(I, n_objects, n_attributes, n_grades, attrs,
                           grades_set, extents, intents, queue_A, queue_B,
                           cols, queue_y.array[cols], Mj, // Pasamos nuestra Mj modificada a los hijos
                           closure_count, intent_count, total, depth + 1, verbose);
  }

  // Liberación de toda la memoria local
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
List FuzzyFCbO(NumericMatrix I,
               NumericVector grades_set,
               StringVector attrs,
               String connection = "standard",
               String name = "Zadeh",
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

  // La matriz original, vacía, que se pasa a la primera llamada
  double* Ny = (double*)calloc((size_t)n_attributes * n_attributes * n_grades, sizeof(double));

  // La primera llamada a la recursión
  FuzzyFastGenerateFrom4(I.begin(), n_objects, n_attributes, n_grades, attrs,
                         grades_set.begin(), &extents, &intents,
                         A, C, 0, -1, Ny,
                         &closure_count, &intent_count, &total, 0, verbose);

  free(Ny);

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&extents);

  timer.step("end");

  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["total"] = total,
    _["closure_count"] = closure_count,
    _["intent_count"] = intent_count,
    _["timer"] = timer);

  return res;
}
