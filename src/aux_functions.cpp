#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;

// void get_column(SparseVector* A,
//                 SparseVector qA,
//                 int id_col) {
//
//   int cont = 0;
//   for (int i = qA.p.array[id_col]; i < qA.p.array[id_col + 1]; i++) {
//
//     insertArray(&(A->i), qA.i.array[i]);
//     insertArray(&(A->x), qA.x.array[i]);
//     cont++;
//
//   }
//
//   insertArray(&(A->p), 0);
//   insertArray(&(A->p), cont);
//
// }

void get_column(SparseVector* A,
                const SparseVector& qA, // <-- Optimización menor: pasar por referencia
                int id_col) {

  // --- PASO 1: REINICIAR EL VECTOR DE DESTINO ---
  // Esta es la línea clave. Borra los datos anteriores de A
  // antes de llenarlo con la nueva columna.
  reinitVector(A);

  // --- PASO 2: COPIAR LOS NUEVOS DATOS ---
  // El resto de la lógica es para copiar los datos de la columna
  // id_col de qA hacia el ahora vacío vector A.

  // Comprobación de seguridad para evitar accesos fuera de límites
  if (id_col + 1 >= qA.p.used) {
    // Si id_col no es válido, A se queda vacío, lo cual es seguro.
    return;
  }

  int start_idx = qA.p.array[id_col];
  int end_idx = qA.p.array[id_col + 1];
  int count = end_idx - start_idx;

  if (count > 0) {
    // Usar memcpy para una copia de memoria masiva y rápida
    memcpy(A->i.array, &qA.i.array[start_idx], count * sizeof(int));
    memcpy(A->x.array, &qA.x.array[start_idx], count * sizeof(double));
  }

  // Actualizar los contadores de elementos usados
  A->i.used = count;
  A->x.used = count;

}

// [[Rcpp::export]]
void print_matrix(NumericMatrix I) {

  for (int i = 0; i < I.nrow(); i++) {

    for (int j = 0; j < I.ncol(); j++) {

      Rcout << I(i, j) << " ";

    }

    Rcout << std::endl;

  }

}

// [[Rcpp::export]]
void print_vector(NumericVector I, int sz) {

  if (sz > I.size())
    sz = I.size();

  for (int i = 0; i < sz; i++) {

    Rcout << I[i] << " ";

  }

  Rcout << std::endl;

}

// [[Rcpp::export]]
double get_element_array(NumericVector I,
                         int i, int j, int k) {

  IntegerVector res(I.attr("dim"));

  int linear_index = k * res[0] * res[1] + j * res[0] + i;

  return I[linear_index];

}

NumericVector zadeh_I(double x, NumericVector y) {

  int n = y.size();
  NumericVector res(y);

  for (int i = 0; i < n; i++) {

    if (x <= y[i]) {

      res[i] = 1;

    }

  }

  return res;

}

void zadeh_I(double x, SparseVector *A) {

  for (size_t i = 0; i < A->i.used; i++) {

    if (A->x.array[i] >= x) {

      A->x.array[i] = 1;

    }

  }

}

void intersect(SparseVector *A, SparseVector B) {

  size_t i, j = 0, to_write = 0;

  for (i = 0; i < A->i.used; i++) {

    int ix = A->i.array[i];

    while ((B.i.array[j] < ix) & (j < B.i.used)) j++;

    if (j >= B.i.used) break;

    if (B.i.array[j] == ix) {

      if (B.x.array[j] < A->x.array[i]) {

        A->x.array[to_write] = B.x.array[j];
        A->i.array[to_write] = ix;
        to_write++;

      } else {

        A->x.array[to_write] = A->x.array[i];
        A->i.array[to_write] = ix;
        to_write++;

      }

    }

  }

  A->i.used = to_write;
  A->x.used = to_write;

  if (A->p.used == 2)
    A->p.array[1] = to_write;

}


/**
 * @name DenseArrayToS4
 * @description Convierte un array denso (almacenado en un DoubleArray)
 * a una matriz dispersa S4 de clase "dgCMatrix", calculando
 * automáticamente el número de columnas.
 *
 * @param data El DoubleArray que contiene los datos de la matriz en
 * formato "column-major".
 * @param nrow El número de filas de la matriz.
 * @returns Un objeto S4 de clase "dgCMatrix".
 */
S4 DenseArrayToS4(const DoubleArray& data, int nrow) {

  if (nrow <= 0) {
    Rcpp::stop("El número de filas (nrow) debe ser positivo.");
  }
  if (data.used % nrow != 0) {
    Rcpp::stop("El número total de elementos no es un múltiplo de nrow.");
  }
  int ncol = data.used / nrow;

  std::vector<int> i_indices;
  std::vector<double> x_values;
  Rcpp::IntegerVector p_pointers(ncol + 1);
  p_pointers[0] = 0;

  for (int j = 0; j < ncol; ++j) { // Bucle por columnas
    int non_zero_count_in_col = 0;
    for (int i = 0; i < nrow; ++i) { // Bucle por filas
      long long current_index = (long long)j * nrow + i;
      double val = data.array[current_index];

      if (val != 0.0) {
        x_values.push_back(val);
        i_indices.push_back(i);
        non_zero_count_in_col++;
      }
    }
    // Correctamente actualiza el puntero de la columna
    p_pointers[j + 1] = p_pointers[j] + non_zero_count_in_col;
  }

  S4 s4_matrix("dgCMatrix");
  s4_matrix.slot("i") = Rcpp::wrap(i_indices);
  s4_matrix.slot("p") = p_pointers;
  s4_matrix.slot("x") = Rcpp::wrap(x_values);
  s4_matrix.slot("Dim") = Rcpp::IntegerVector::create(nrow, ncol);
  s4_matrix.slot("Dimnames") = Rcpp::List::create(R_NilValue, R_NilValue);
  s4_matrix.slot("factors") = Rcpp::List();

  return s4_matrix;
}
