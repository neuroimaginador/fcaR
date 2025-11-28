#include <Rcpp.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;

// Estructura ligera para grafos dispersos (Listas de Adyacencia)
struct AdjacencyGraph {
  int n;
  std::vector<std::vector<int>> successors; // i -> {j1, j2, ...}

  // Construir desde formato CSC (dgCMatrix)
  AdjacencyGraph(int n_nodes, int* i_ptr, int* p_ptr) : n(n_nodes) {
    successors.resize(n);
    for (int col = 0; col < n; ++col) {
      for (int k = p_ptr[col]; k < p_ptr[col+1]; ++k) {
        int row = i_ptr[k];
        if (row != col) { // Ignorar diagonal (reflexividad)
          // En dgCMatrix (i, p) almacena columnas.
          // Si M[row, col] = 1, significa row -> col ??
          // OJO: En fcaR normalmente las filas son objetos y col atributos.
          // Pero si esto es una matriz de adyacencia de retículo, M[i, j]=1 suele ser i <= j.
          // dgCMatrix almacena por columnas. p[j] tiene las filas i tal que M[i, j]=1.
          // Es decir, i <= j.
          // Queremos saber los sucesores de i.
          // successors[i] debería tener a j.
          // Aquí tenemos row <= col.
          successors[row].push_back(col);
        }
      }
    }
    // Ordenar para búsquedas rápidas
    for(auto& s : successors) std::sort(s.begin(), s.end());
  }

  bool has_edge(int u, int v) const {
    const auto& s = successors[u];
    return std::binary_search(s.begin(), s.end(), v);
  }
};

// [[Rcpp::export]]
List reduce_transitivity_cpp(SEXP sp_i, SEXP sp_p, SEXP dim) {
  int* i_ptr = INTEGER(sp_i);
  int* p_ptr = INTEGER(sp_p);
  int* dim_ptr = INTEGER(dim);
  int n = dim_ptr[0];

  // 1. Construir Grafo (solo aristas estrictas, sin diagonal)
  AdjacencyGraph G(n, i_ptr, p_ptr);

  // 2. Filtrar Aristas Redundantes (Transitive Reduction)
  // Una arista u -> v se mantiene SI Y SOLO SI NO existe k tal que u -> k -> v

  std::vector<int> res_i, res_p;
  res_p.push_back(0);

  // Iteramos por COLUMNAS (v) para construir el resultado en formato CSC directamente
  for (int v = 0; v < n; ++v) {
    int count = 0;

    // Recuperar los predecesores (u) de v desde la matriz original
    // En formato CSC, la columna v contiene todas las filas u tales que u -> v
    int start = p_ptr[v];
    int end = p_ptr[v+1];

    for (int k = start; k < end; ++k) {
      int u = i_ptr[k];

      if (u == v) continue; // Diagonal fuera

      // Comprobar transitividad: ¿Existe camino indirecto u -> ... -> v?
      // Basta con buscar un k tal que u -> k y k -> v.
      // Para optimizar: Iteramos los sucesores de u.

      bool redundant = false;
      const std::vector<int>& u_succs = G.successors[u];

      for (int intermediate : u_succs) {
        if (intermediate == v) continue; // Es la arista directa

        // Si u -> intermediate y intermediate -> v (existe en el grafo original)
        // Nota: G.has_edge busca en la estructura optimizada
        // Para saber si intermediate -> v, podríamos mirar la matriz original (columna v),
        // pero G.has_edge(intermediate, v) es lo conceptualmente correcto.

        if (G.has_edge(intermediate, v)) {
          redundant = true;
          break;
        }
      }

      if (!redundant) {
        res_i.push_back(u);
        count++;
      }
    }
    res_p.push_back(res_p.back() + count);
  }

  // 3. Retornar lista para new("dgCMatrix")
  return List::create(
    Named("i") = wrap(res_i),
    Named("p") = wrap(res_p),
    Named("Dim") = dim
  );
}
