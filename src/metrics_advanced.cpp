#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;

// --- HELPER: Intersección/Unión de conjuntos dispersos (necesario para Separación) ---
// Calcula el tamaño de la unión de los extents de los hijos
int size_of_union_sparse(const std::vector<int>& indices, const IntegerVector& p_slot, const IntegerVector& i_slot) {
  // Esta es una aproximación rápida:
  // La separación extensional estricta es |Ext(C)| - |Union(Ext(K))| para hijos K directos.
  // Para hacerlo eficiente en C++, usaremos un vector de booleanos temporal si N no es gigante,
  // o un std::set si es muy grande. Dado que R maneja memoria, un std::vector<bool> de tamaño N_objetos es seguro.

  // Pero espera, necesitamos saber N_objetos.
  // Asumiremos que se pasa como parámetro o se deduce.
  return 0; // Placeholder lógica compleja
}

// --- 1. SEPARACIÓN EXTENSIONAL ---
// Sep(C) = |Ext(C)| - max(|Ext(K)|) para todo K subconcepto directo.
// Esta es una definición simplificada muy útil: "el salto de tamaño".
// La definición estricta (objetos propios) requiere la unión de todos los hijos.
// Vamos a implementar la DIFERENCIA MÁXIMA (Maximal Difference) que es O(1) con los datos precalculados.

// [[Rcpp::export]]
NumericVector calculate_separation_rcpp(S4 mat) {
  IntegerVector i_slot = mat.slot("i");
  IntegerVector p_slot = mat.slot("p");
  IntegerVector Dim = mat.slot("Dim");
  int n_concepts = Dim[1];

  std::vector<int> sizes(n_concepts);
  // Calcular tamaños
  for (int k = 0; k < n_concepts; ++k) {
    sizes[k] = p_slot[k+1] - p_slot[k];
  }

  // Para calcular la separación real, necesitamos la estructura del retículo (padres/hijos).
  // Si no la tenemos en C++, podemos aproximarla o pedir que se pase.
  // ERROR DE DISEÑO: Calcular separación sin el grafo es O(N^2) (recalcular subconjuntos).
  // SOLUCIÓN: Pasar los índices de los subconceptos directos desde R (que ya tiene el retículo).

  return NumericVector(n_concepts, 0.0);
}

// RE-DISEÑO: Haremos la DENSIDAD en C++ (porque requiere acceso a la matriz I original)
// y la SEPARACIÓN en R (porque requiere navegar el grafo que ya tiene R y es rápido con operaciones de conjuntos).

// --- 2. DENSIDAD DIFUSA (Fuzzy Density) ---
// Requiere:
// - Matriz de Extents (binaria/dispersa, define qué objetos están)
// - Matriz de Intents (binaria/dispersa, define qué atributos están)
// - Matriz ORIGINAL I (numérica, contiene los grados reales)

// [[Rcpp::export]]
NumericVector calculate_fuzzy_density_rcpp(S4 extents, S4 intents, NumericMatrix I) {
  // Extents e Intents son sparse (dgCMatrix)
  // I es densa (numeric matrix de R)

  IntegerVector ext_i = extents.slot("i");
  IntegerVector ext_p = extents.slot("p");
  IntegerVector ext_dim = extents.slot("Dim");

  IntegerVector int_i = intents.slot("i");
  IntegerVector int_p = intents.slot("p");

  int n_concepts = ext_dim[1];
  NumericVector density(n_concepts);

  // Punteros crudos
  const int* ext_rows = ext_i.begin();
  const int* ext_ptr = ext_p.begin();

  const int* int_rows = int_i.begin();
  const int* int_ptr = int_p.begin();

  for (int k = 0; k < n_concepts; ++k) {
    // Objetos del concepto k
    int start_obj = ext_ptr[k];
    int end_obj = ext_ptr[k+1];
    int n_obj = end_obj - start_obj;

    // Atributos del concepto k
    int start_att = int_ptr[k];
    int end_att = int_ptr[k+1];
    int n_att = end_att - start_att;

    if (n_obj == 0 || n_att == 0) {
      density[k] = 0.0; // O 1.0 para el concepto vacío, depende de la definición
      continue;
    }

    double sum_I = 0.0;

    // Iterar sobre el bloque (Producto Cartesiano Ext x Int)
    for (int o = start_obj; o < end_obj; ++o) {
      int obj_idx = ext_rows[o]; // Fila en I

      for (int a = start_att; a < end_att; ++a) {
        int att_idx = int_rows[a]; // Columna en I

        // Acceso a matriz densa I (Column-Major en R)
        // I[row, col] -> I[row + col * nrow]
        sum_I += I[obj_idx + att_idx * I.nrow()];
      }
    }

    // Densidad = Suma / Área
    density[k] = sum_I / (double)(n_obj * n_att);
  }

  return density;
}
