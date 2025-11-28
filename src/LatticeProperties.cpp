#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <Rinternals.h>

using namespace Rcpp;

// =============================================================================
// --- HELPERS DE MEMORIA (Sparse -> Dense Lookup) ---
// =============================================================================
// Para verificar propiedades en O(1) por acceso, "densificamos" temporalmente
// las tablas de operaciones en C++. Dado que las tablas Meet/Join son densas
// (tienen valor para todo par), esto es lo más eficiente.

std::vector<int> densify_matrix(int* i_ptr, int* p_ptr, double* x_ptr, int n) {
  // Crea un vector plano NxN.
  // Inicializamos con -1 (error) o 0. Como R usa base-1, 0 es seguro como "vacío"
  std::vector<int> dense(n * n, 0);

  for (int col = 0; col < n; ++col) {
    int start = p_ptr[col];
    int end = p_ptr[col+1];

    for (int k = start; k < end; ++k) {
      int row = i_ptr[k];
      // Si hay valor en x, lo usamos. Si no (patrón ngCMatrix), asumimos 1 (para adj/covering)
      // Para Meet/Join, necesitamos el valor x (índice del concepto).
      int val = (x_ptr != nullptr) ? (int)x_ptr[k] : 1;

      // Column-major order: M[row, col]
      dense[col * n + row] = val;
    }
  }
  return dense;
}

// Helper para Adyacencia (ngCMatrix, sin slot x)
std::vector<int> densify_pattern(int* i_ptr, int* p_ptr, int n) {
  return densify_matrix(i_ptr, p_ptr, nullptr, n);
}

// Helper de conjuntos para la generación
inline std::vector<int> intersect_sorted(const std::vector<int>& A, const std::vector<int>& B) {
  std::vector<int> res;
  res.reserve(std::min(A.size(), B.size()));
  auto itA = A.begin(); auto itB = B.begin();
  while (itA != A.end() && itB != B.end()) {
    if (*itA < *itB) ++itA;
    else if (*itB < *itA) ++itB;
    else { res.push_back(*itA); ++itA; ++itB; }
  }
  return res;
}

// =============================================================================
// --- 1. GENERADOR DE TABLAS MEET / JOIN ---
// =============================================================================

// [[Rcpp::export]]
List compute_lattice_tables_cpp(SEXP adj_i, SEXP adj_p, SEXP dim) {
  int* i_ptr = INTEGER(adj_i);
  int* p_ptr = INTEGER(adj_p);
  int* dim_ptr = INTEGER(dim);
  int n = dim_ptr[0];

  // 1. Construir Downsets (Columnas de Adj) y Upsets (Filas de Adj)
  std::vector<std::vector<int>> downsets(n);
  std::vector<std::vector<int>> upsets(n);
  std::vector<int> sizes(n);

  for (int col = 0; col < n; ++col) {
    int start = p_ptr[col];
    int end = p_ptr[col+1];
    downsets[col].reserve(end - start);

    for (int k = start; k < end; ++k) {
      int row = i_ptr[k];
      downsets[col].push_back(row); // i <= j (si Adj es orden)
      upsets[row].push_back(col);   // j >= i
    }
    std::sort(downsets[col].begin(), downsets[col].end());
    sizes[col] = downsets[col].size(); // Altura relativa
  }
  for(int i=0; i<n; ++i) std::sort(upsets[i].begin(), upsets[i].end());

  // 2. Calcular Tablas
  // Usamos formato "Triplet" temporal para construcción fácil
  std::vector<int> m_i, m_p(n + 1, 0), m_x;
  std::vector<int> j_i, j_p(n + 1, 0), j_x;

  m_x.reserve(n*n); m_i.reserve(n*n);
  j_x.reserve(n*n); j_i.reserve(n*n);

  // Iteramos por COLUMNAS (y) luego FILAS (x) para formato CSC
  for (int y = 0; y < n; ++y) {
    for (int x = 0; x < n; ++x) {

      // MEET: Max(Downset(x) Intersect Downset(y))
      std::vector<int> common_lower = intersect_sorted(downsets[x], downsets[y]);
      int meet_val = -1;
      int max_s = -1;

      for(int c : common_lower) {
        if(sizes[c] > max_s) { max_s = sizes[c]; meet_val = c; }
      }

      // JOIN: Min(Upset(x) Intersect Upset(y))
      std::vector<int> common_upper = intersect_sorted(upsets[x], upsets[y]);
      int join_val = -1;
      int min_s = n + 999;

      for(int c : common_upper) {
        if(sizes[c] < min_s) { min_s = sizes[c]; join_val = c; }
      }

      // Guardamos (Indices + 1 para R)
      // Guardamos TODOS, incluso si es 0 (aunque no debería haber concepto 0 en R)
      // Al ser matriz densa de operaciones, la estructura es trivial
      m_i.push_back(x); m_x.push_back(meet_val + 1);
      j_i.push_back(x); j_x.push_back(join_val + 1);
    }
    m_p[y+1] = m_p[y] + n;
    j_p[y+1] = j_p[y] + n;
  }

  // Retornar listas listas para 'new("dgCMatrix")'
  List l_meet = List::create(Named("i")=wrap(m_i), Named("p")=wrap(m_p), Named("x")=wrap(m_x), Named("Dim")=dim);
  List l_join = List::create(Named("i")=wrap(j_i), Named("p")=wrap(j_p), Named("x")=wrap(j_x), Named("Dim")=dim);

  return List::create(Named("meet") = l_meet, Named("join") = l_join);
}

// =============================================================================
// --- 2. VERIFICADORES DE PROPIEDADES ---
// =============================================================================

// [[Rcpp::export]]
bool check_distributivity_sparse(SEXP m_i, SEXP m_p, SEXP m_x,
                                 SEXP j_i, SEXP j_p, SEXP j_x, SEXP dim) {
  int n = INTEGER(dim)[0];

  // Densificar para acceso O(1)
  std::vector<int> M = densify_matrix(INTEGER(m_i), INTEGER(m_p), REAL(m_x), n);
  std::vector<int> J = densify_matrix(INTEGER(j_i), INTEGER(j_p), REAL(j_x), n);

  // x ^ (y v z) == (x ^ y) v (x ^ z)
  // Usamos Base-0 internamente, restamos 1 a los valores leídos de las tablas
  for (int x = 0; x < n; ++x) {
    for (int y = 0; y < n; ++y) {
      for (int z = 0; z < n; ++z) {

        int y_v_z = J[z * n + y] - 1;
        int lhs   = M[y_v_z * n + x] - 1;

        int x_m_y = M[y * n + x] - 1;
        int x_m_z = M[z * n + x] - 1;
        int rhs   = J[x_m_z * n + x_m_y] - 1;

        if (lhs != rhs) return false;
      }
    }
  }
  return true;
}

// [[Rcpp::export]]
bool check_modularity_sparse(SEXP m_i, SEXP m_p, SEXP m_x,
                             SEXP j_i, SEXP j_p, SEXP j_x, SEXP dim) {
  int n = INTEGER(dim)[0];
  std::vector<int> M = densify_matrix(INTEGER(m_i), INTEGER(m_p), REAL(m_x), n);
  std::vector<int> J = densify_matrix(INTEGER(j_i), INTEGER(j_p), REAL(j_x), n);

  // x <= z  =>  x v (y ^ z) == (x v y) ^ z
  for (int x = 0; x < n; ++x) {
    for (int z = 0; z < n; ++z) {
      // Check orden: x <= z  <==> x ^ z == x
      if ((M[z * n + x] - 1) == x) {
        for (int y = 0; y < n; ++y) {
          int y_m_z = M[z * n + y] - 1;
          int lhs   = J[y_m_z * n + x] - 1;

          int x_v_y = J[y * n + x] - 1;
          int rhs   = M[z * n + x_v_y] - 1;

          if (lhs != rhs) return false;
        }
      }
    }
  }
  return true;
}

// [[Rcpp::export]]
bool check_semimodularity_sparse(SEXP m_i, SEXP m_p, SEXP m_x,
                                 SEXP j_i, SEXP j_p, SEXP j_x,
                                 SEXP cov_i, SEXP cov_p, SEXP dim) {
  int n = INTEGER(dim)[0];
  std::vector<int> M = densify_matrix(INTEGER(m_i), INTEGER(m_p), REAL(m_x), n);
  std::vector<int> J = densify_matrix(INTEGER(j_i), INTEGER(j_p), REAL(j_x), n);

  // Covering Matrix (Binaria/Pattern)
  std::vector<int> Cov = densify_pattern(INTEGER(cov_i), INTEGER(cov_p), n);

  // Semimodular Superior: Si x cubre a (x^y), entonces (xvy) cubre a y.
  for (int x = 0; x < n; ++x) {
    for (int y = 0; y < n; ++y) {
      if (x == y) continue;

      int meet_xy = M[y * n + x] - 1;

      // Check: x covers meet_xy?  Cov(row=meet, col=x) == 1
      // Cov está en Column-major: Cov[col*n + row]
      if (Cov[x * n + meet_xy]) {

        int join_xy = J[y * n + x] - 1;

        // Check: join_xy covers y? Cov(row=y, col=join)
        if (!Cov[join_xy * n + y]) {
          return false;
        }
      }
    }
  }
  return true;
}

// [[Rcpp::export]]
bool check_atomicity_sparse(SEXP adj_i, SEXP adj_p,
                            SEXP cov_i, SEXP cov_p, SEXP dim) {
  int n = INTEGER(dim)[0];
  std::vector<int> Adj = densify_pattern(INTEGER(adj_i), INTEGER(adj_p), n);
  std::vector<int> Cov = densify_pattern(INTEGER(cov_i), INTEGER(cov_p), n);

  // 1. Encontrar Bottom (Elemento que es <= todos)
  // En Adj(i, j)=1 <=> i <= j. Bottom es la fila i con todo 1s.
  // Ojo: si pasamos dgCMatrix, solo tiene los 1s.

  int bottom = -1;
  for (int i = 0; i < n; ++i) {
    bool is_bot = true;
    for (int j = 0; j < n; ++j) {
      // Adj[col*n + row]
      if (Adj[j * n + i] == 0) { // i <= j ?
        is_bot = false; break;
      }
    }
    if (is_bot) { bottom = i; break; }
  }

  if (bottom == -1) return false; // Retículo mal formado?

  // 2. Identificar Átomos (Cubren a bottom)
  std::vector<int> atoms;
  for (int i = 0; i < n; ++i) {
    // Cov(bottom, i) -> i cubre a bottom
    if (Cov[i * n + bottom]) atoms.push_back(i);
  }

  if (atoms.empty() && n > 1) return false;

  // 3. Verificar: Todo x > bottom debe contener un átomo (a <= x)
  for (int x = 0; x < n; ++x) {
    if (x == bottom) continue;

    bool has_atom_below = false;
    for (int a : atoms) {
      // Check a <= x (Adj(row=a, col=x))
      if (Adj[x * n + a]) {
        has_atom_below = true;
        break;
      }
    }
    if (!has_atom_below) return false;
  }

  return true;
}
