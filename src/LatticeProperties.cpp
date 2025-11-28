#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <map>

using namespace Rcpp;

// =============================================================================
// --- MOTOR LÓGICO DE RETÍCULOS ---
// =============================================================================

// Clase ligera para operaciones de bits (para filas de la matriz de adyacencia)
struct BitRow {
  std::vector<uint64_t> blocks;
  size_t n_bits;

  BitRow(size_t n) : n_bits(n) {
    blocks.resize((n + 63) / 64, 0);
  }

  void set(int i) { blocks[i/64] |= (1ULL << (i%64)); }
  bool get(int i) const { return (blocks[i/64] & (1ULL << (i%64))) != 0; }

  // Intersección (AND)
  void intersect(const BitRow& other) {
    for(size_t i=0; i<blocks.size(); ++i) blocks[i] &= other.blocks[i];
  }

  // Unión (OR)
  bool unite(const BitRow& other) {
    bool changed = false;
    for(size_t i=0; i<blocks.size(); ++i) {
      uint64_t old = blocks[i];
      blocks[i] |= other.blocks[i];
      if (blocks[i] != old) changed = true;
    }
    return changed;
  }

  bool equals(const BitRow& other) const {
    return blocks == other.blocks;
  }
};

// --- 1. Verificar desde Matriz de Adyacencia ---
// [[Rcpp::export]]
List check_lattice_properties_adjacency(IntegerMatrix M) {
  int n = M.nrow();
  if (M.ncol() != n) stop("Adjacency matrix must be square.");

  // 1. Convertir a BitRows y Calcular Cierre Transitivo (Warshall)
  // Necesitamos la relación de orden completa (<=), no solo covering.
  std::vector<BitRow> Order(n, BitRow(n));

  // Cargar inicial
  for(int i=0; i<n; ++i) {
    Order[i].set(i); // Reflexiva (i <= i)
    for(int j=0; j<n; ++j) {
      if (M(i, j) != 0) Order[i].set(j);
    }
  }

  // Warshall: Si i <= k y k <= j, entonces i <= j
  for(int k=0; k<n; ++k) {
    for(int i=0; i<n; ++i) {
      if (Order[i].get(k)) {
        Order[i].unite(Order[k]);
      }
    }
  }

  // 2. Precalcular Tablas Meet/Join
  // Meet(i, j) = El mayor k tal que k <= i AND k <= j
  // Join(i, j) = El menor k tal que i <= k AND j <= k

  // Para acelerar, precalculamos columnas (Downsets y Upsets)
  // Order[i] es el Upset de i (cosas mayores que i)
  // Necesitamos Downsets para el Meet
  std::vector<BitRow> Downsets(n, BitRow(n));
  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {
      if (Order[i].get(j)) { // i <= j
        Downsets[j].set(i);
      }
    }
  }

  std::vector<std::vector<int>> meet_table(n, std::vector<int>(n));
  std::vector<std::vector<int>> join_table(n, std::vector<int>(n));

  for(int i=0; i<n; ++i) {
    for(int j=0; j<n; ++j) {

      // --- MEET (Infimo) ---
      // Intersección de Downsets (Cotas inferiores comunes)
      BitRow lower_bounds = Downsets[i];
      lower_bounds.intersect(Downsets[j]);

      // Buscar el elemento máximo en lower_bounds
      // En un retículo, el conjunto de cotas inferiores es un ideal principal generado por el Meet.
      // Es decir, existe un m tal que Downsets[m] == lower_bounds.
      int meet_val = -1;
      for(int k=0; k<n; ++k) {
        if (Downsets[k].equals(lower_bounds)) {
          meet_val = k;
          break;
        }
      }
      if (meet_val == -1) stop("Structure is not a Lattice (Meet undefined).");
      meet_table[i][j] = meet_val;

      // --- JOIN (Supremo) ---
      // Intersección de Upsets (Cotas superiores comunes)
      BitRow upper_bounds = Order[i];
      upper_bounds.intersect(Order[j]);

      int join_val = -1;
      for(int k=0; k<n; ++k) {
        if (Order[k].equals(upper_bounds)) {
          join_val = k;
          break;
        }
      }
      if (join_val == -1) stop("Structure is not a Lattice (Join undefined).");
      join_table[i][j] = join_val;
    }
  }

  // 3. Verificar Propiedades
  bool is_distributive = true;
  bool is_modular = true;

  for (int x = 0; x < n; ++x) {
    for (int y = 0; y < n; ++y) {
      for (int z = 0; z < n; ++z) {

        // Distributividad: x ^ (y v z) == (x ^ y) v (x ^ z)
        if (is_distributive) {
          int lhs = meet_table[x][join_table[y][z]];
          int rhs = join_table[meet_table[x][y]][meet_table[x][z]];
          if (lhs != rhs) is_distributive = false;
        }

        // Modularidad: x <= z => x v (y ^ z) == (x v y) ^ z
        if (is_modular) {
          if (meet_table[x][z] == x) { // x <= z
            int lhs = join_table[x][meet_table[y][z]];
            int rhs = meet_table[join_table[x][y]][z];
            if (lhs != rhs) is_modular = false;
          }
        }

        if (!is_distributive && !is_modular) goto done;
      }
    }
  }

  done:
    return List::create(Named("distributive") = is_distributive, Named("modular") = is_modular);
}

