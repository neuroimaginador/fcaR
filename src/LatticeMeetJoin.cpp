#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <Rinternals.h>

using namespace Rcpp;

// =============================================================================
// --- HELPERS DISPERSOS ---
// =============================================================================

// Intersección rápida de dos vectores ORDENADOS
// Devuelve los elementos comunes
inline std::vector<int> intersect_sorted(const std::vector<int>& A, const std::vector<int>& B) {
  std::vector<int> res;
  // Heurística de reserva: el tamaño será como máximo el del menor vector
  res.reserve(std::min(A.size(), B.size()));

  auto itA = A.begin();
  auto itB = B.begin();
  auto endA = A.end();
  auto endB = B.end();

  while (itA != endA && itB != endB) {
    if (*itA < *itB) {
      ++itA;
    } else if (*itB < *itA) {
      ++itB;
    } else {
      res.push_back(*itA);
      ++itA;
      ++itB;
    }
  }
  return res;
}

// =============================================================================
// --- MOTOR DE OPERACIONES DE RETÍCULO ---
// =============================================================================

// [[Rcpp::export]]
List compute_meet_join_cpp(SEXP sp_i, SEXP sp_p, SEXP dim) {

  // 1. LECTURA DE PUNTEROS (Zero Copy)
  int* i_ptr = INTEGER(sp_i);
  int* p_ptr = INTEGER(sp_p);
  int* dim_ptr = INTEGER(dim);

  int n_concepts = dim_ptr[0]; // Matriz cuadrada NxN
  // n_concepts es el número de conceptos

  // 2. CONSTRUIR ESTRUCTURAS DE GRAFO (Adjacency Lists)
  // Downsets: i <= j. Coincide con las columnas de la matriz de adyacencia.
  // Upsets: i >= j. Coincide con las filas (o columnas de la transpuesta).

  std::vector<std::vector<int>> downsets(n_concepts);
  std::vector<std::vector<int>> upsets(n_concepts);
  std::vector<int> downset_sizes(n_concepts, 0);

  // Leer matriz CSC (Compressed Sparse Column)
  // Columna 'col' contiene los índices 'row' tal que row <= col (si Adjacency es i <= j)
  // Por tanto, la columna 'col' es el Downset de 'col'.

  for (int col = 0; col < n_concepts; ++col) {
    int start = p_ptr[col];
    int end = p_ptr[col+1];

    downsets[col].reserve(end - start);

    for (int k = start; k < end; ++k) {
      int row = i_ptr[k];

      // Guardar en Downset
      downsets[col].push_back(row);

      // Construir Upset (Transpuesta) al vuelo
      // Si row <= col, entonces col >= row (col está en el Upset de row)
      upsets[row].push_back(col);
    }
    // Asegurar orden para intersecciones (dgCMatrix suele estar ordenada, pero por seguridad)
    std::sort(downsets[col].begin(), downsets[col].end());

    // Guardar tamaño para encontrar máximos/mínimos rápidamente
    downset_sizes[col] = downsets[col].size();
  }

  // Ordenar Upsets también
  for(int i=0; i<n_concepts; ++i) {
    std::sort(upsets[i].begin(), upsets[i].end());
  }

  // 3. CALCULAR TABLAS MEET Y JOIN
  // Salida formato CSC: vectors x, i, p
  // Como las tablas son densas (NxN), p es trivial (0, N, 2N...), pero lo generamos igual.

  // Meet
  std::vector<int> meet_x, meet_i, meet_p;
  meet_p.push_back(0);
  meet_x.reserve(n_concepts * n_concepts);
  meet_i.reserve(n_concepts * n_concepts);

  // Join
  std::vector<int> join_x, join_i, join_p;
  join_p.push_back(0);
  join_x.reserve(n_concepts * n_concepts);
  join_i.reserve(n_concepts * n_concepts);

  // Iteramos por COLUMNAS (j) y luego FILAS (i) para seguir el formato CSC
  for (int j = 0; j < n_concepts; ++j) {
    for (int i = 0; i < n_concepts; ++i) {

      // --- MEET (i, j) ---
      // Intersección de Downsets (Cotas inferiores comunes)
      std::vector<int> lower_bounds = intersect_sorted(downsets[i], downsets[j]);

      // El Ínfimo es el elemento en lower_bounds con MAYOR tamaño de downset (el más alto)
      int meet_val = -1;
      int max_sz = -1;

      for (int k : lower_bounds) {
        if (downset_sizes[k] > max_sz) {
          max_sz = downset_sizes[k];
          meet_val = k;
        }
      }

      // Guardar (convertir a base-1 para R si es necesario, o mantener 0-based y sumar en R)
      // dgCMatrix almacena valores en 'x'. Aquí almacenamos el ÍNDICE del concepto.
      // Usaremos base-1 para que sea intuitivo en R (Concepto 1..N)
      meet_i.push_back(i);
      meet_x.push_back(meet_val + 1);


      // --- JOIN (i, j) ---
      // Intersección de Upsets (Cotas superiores comunes)
      std::vector<int> upper_bounds = intersect_sorted(upsets[i], upsets[j]);

      // El Supremo es el elemento en upper_bounds con MENOR tamaño de downset (el más bajo)
      int join_val = -1;
      int min_sz = 99999999;

      for (int k : upper_bounds) {
        if (downset_sizes[k] < min_sz) {
          min_sz = downset_sizes[k];
          join_val = k;
        }
      }

      join_i.push_back(i);
      join_x.push_back(join_val + 1);
    }

    // Actualizar punteros de columna (siempre +N porque es denso)
    meet_p.push_back(meet_p.back() + n_concepts);
    join_p.push_back(join_p.back() + n_concepts);
  }

  // 4. EMPAQUETAR SALIDA
  // Devolvemos listas listas para `new("dgCMatrix", ...)`

  List meet_list = List::create(
    Named("i") = wrap(meet_i),
    Named("p") = wrap(meet_p),
    Named("x") = wrap(meet_x),
    Named("Dim") = IntegerVector::create(n_concepts, n_concepts)
  );

  List join_list = List::create(
    Named("i") = wrap(join_i),
    Named("p") = wrap(join_p),
    Named("x") = wrap(join_x),
    Named("Dim") = IntegerVector::create(n_concepts, n_concepts)
  );

  return List::create(Named("meet") = meet_list, Named("join") = join_list);
}
