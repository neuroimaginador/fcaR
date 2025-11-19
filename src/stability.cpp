#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// Helper: Comprobar si A es subconjunto de B usando los arrays crudos de la matriz dispersa.
// A_ptr y B_ptr apuntan al inicio de los índices de fila en el vector 'i'.
// A_len y B_len son el número de elementos (non-zeros) en esa columna.
// Como las matrices dispersas en R (Matrix package) tienen los índices ordenados dentro de cada columna,
// podemos hacer esto en O(N).
bool is_subset_sparse(const int* A_ptr, int A_len, const int* B_ptr, int B_len) {
  int i = 0, j = 0;
  while (i < A_len && j < B_len) {
    if (A_ptr[i] < B_ptr[j]) {
      // A tiene un elemento menor que el actual de B.
      // Como están ordenados, este elemento de A no puede estar en B.
      return false;
    }
    if (A_ptr[i] == B_ptr[j]) {
      i++; // Coincidencia, avanzamos A
    }
    j++; // Avanzamos B siempre
  }
  // Si hemos recorrido todo A, es subconjunto
  return (i == A_len);
}

// [[Rcpp::export]]
NumericVector calculate_stability_sparse_rcpp(S4 mat) {
  // Accedemos a los slots de la matriz dispersa (dgCMatrix / ngCMatrix)
  // 'i': Vector de índices de fila (0-based en C++, R lo maneja internamente)
  // 'p': Punteros de columna (dónde empieza y acaba cada columna en 'i')
  // 'Dim': Dimensiones

  IntegerVector i_slot = mat.slot("i");
  IntegerVector p_slot = mat.slot("p");
  IntegerVector Dim = mat.slot("Dim");

  int n_concepts = Dim[1]; // Las columnas son los conceptos

  // Punteros a los datos crudos para máxima velocidad
  const int* i_ptr = i_slot.begin();
  const int* p_ptr = p_slot.begin();

  // Estructuras auxiliares
  std::vector<int> sizes(n_concepts);
  std::vector<int> order(n_concepts);
  NumericVector stability(n_concepts, 1.0); // Inicialmente 1.0

  // 1. Calcular tamaños (Support) y preparar orden
  for (int k = 0; k < n_concepts; ++k) {
    sizes[k] = p_ptr[k+1] - p_ptr[k]; // Tamaño = fin - inicio
    order[k] = k;
  }

  // 2. Ordenar conceptos por tamaño de extensión (ascendente)
  // Necesario para el algoritmo bottom-up
  std::sort(order.begin(), order.end(), [&](int a, int b) {
    return sizes[a] < sizes[b];
  });

  // 3. Cálculo Bottom-Up de Estabilidad
  // Stab(C) = 1 - Sum_{K < C} Stab(K) * 2^(|Ext_K| - |Ext_C|)

  for (int k = 0; k < n_concepts; ++k) {
    int idx_sub = order[k]; // Concepto 'K' (Hijo potencial)
    int size_sub = sizes[idx_sub];

    // Puntero al inicio de los objetos de K en el array 'i'
    const int* sub_indices = i_ptr + p_ptr[idx_sub];

    // Solo comparamos con conceptos más grandes (superconceptos potenciales)
    for (int m = k + 1; m < n_concepts; ++m) {
      int idx_super = order[m]; // Concepto 'C' (Padre potencial)
      int size_super = sizes[idx_super];

      // Optimización: Si tienen el mismo tamaño, no pueden ser subconjuntos propios
      if (size_sub == size_super) continue;

      const int* super_indices = i_ptr + p_ptr[idx_super];

      // Comprobamos si Ext(K) es subconjunto de Ext(C)
      if (is_subset_sparse(sub_indices, size_sub, super_indices, size_super)) {

        double diff = (double)size_sub - (double)size_super; // Negativo
        // factor = 2^(diff) = 1 / 2^(|C| - |K|)
        double factor = std::pow(2.0, diff);

        stability[idx_super] -= stability[idx_sub] * factor;
      }
    }
  }

  // Limpieza de errores de punto flotante
  for(int k=0; k<n_concepts; ++k) {
    if(stability[k] < 0) stability[k] = 0;
    if(stability[k] > 1) stability[k] = 1;
  }

  return stability;
}
