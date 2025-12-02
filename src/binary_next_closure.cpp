#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <algorithm>
#include <chrono>

using namespace Rcpp;

// =============================================================================
// --- ESTRUCTURAS Y AYUDANTES DE BAJO NIVEL (Bitwise) ---
// =============================================================================

// Constantes para aritmética de bits
static const size_t BLOCK_SIZE = 64;
static const uint64_t ALL_ONES = 0xFFFFFFFFFFFFFFFFULL;

// Helper para obtener el índice del bloque y el bit
inline void get_bit_coords(int index, size_t &block, uint64_t &mask) {
  block = index / BLOCK_SIZE;
  mask = 1ULL << (index % BLOCK_SIZE);
}

// Estructura ligera para manejar conjuntos de bits
struct BitSet {
  std::vector<uint64_t> blocks;
  size_t n_bits;
  size_t n_blocks;

  BitSet(size_t n) : n_bits(n) {
    n_blocks = (n + 63) / 64;
    blocks.resize(n_blocks, 0);
  }

  void clear() {
    std::fill(blocks.begin(), blocks.end(), 0);
  }

  // Copia rápida
  void copyFrom(const BitSet& other) {
    if (n_blocks != other.n_blocks) blocks.resize(other.n_blocks);
    std::copy(other.blocks.begin(), other.blocks.end(), blocks.begin());
    n_bits = other.n_bits;
    n_blocks = other.n_blocks;
  }

  // Set bit
  inline void set(int index) {
    blocks[index / 64] |= (1ULL << (index % 64));
  }

  // Unset bit
  inline void unset(int index) {
    blocks[index / 64] &= ~(1ULL << (index % 64));
  }

  // Check bit
  inline bool get(int index) const {
    return (blocks[index / 64] & (1ULL << (index % 64))) != 0;
  }

  // Comprueba si 'this' es subconjunto de 'other'
  inline bool is_subset_of(const BitSet& other) const {
    for (size_t i = 0; i < n_blocks; ++i) {
      if ((blocks[i] & ~other.blocks[i]) != 0) return false;
    }
    return true;
  }

  // Union: this = this OR other
  inline void set_union(const BitSet& other) {
    for (size_t i = 0; i < n_blocks; ++i) {
      blocks[i] |= other.blocks[i];
    }
  }

  // Difference: res = A \ B
  inline void set_difference(const BitSet& A, const BitSet& B) {
    for (size_t i = 0; i < n_blocks; ++i) {
      blocks[i] = A.blocks[i] & ~B.blocks[i];
    }
  }

  // Equality
  inline bool equals(const BitSet& other) const {
    for (size_t i = 0; i < n_blocks; ++i) {
      if (blocks[i] != other.blocks[i]) return false;
    }
    return true;
  }

  // Pone a cero todos los bits >= index
  void clear_after(int index) {
    size_t block = index / 64;
    size_t bit = index % 64;

    // Limpiar la parte alta del bloque actual
    if (bit < 63) {
      uint64_t mask = (1ULL << (bit + 1)) - 1;
      // Queremos mantener desde 0 hasta bit (inclusive), borrar el resto
      // No, wait. index es el primero que queremos BORRAR o el ultimo mantener?
      // "remove elements > i" -> elements from i+1 onwards.
      // Aquí index es el "pivot". Mantenemos < index. Borramos >= index.
      // Si index = 0, borramos todo.

      if (index == 0) {
        clear();
        return;
      }

      // Recalcular mascara para mantener bits 0..(index-1)
      // Ejemplo index=1. bit=1. block=0. Queremos mantener bit 0.
      mask = (1ULL << bit) - 1;
      blocks[block] &= mask;
    } else {
      // Si el bit es 63 (o más), en este bloque no borramos nada (index está en el siguiente o es fin)
      // Pero como index < 64 entra aqui, si index es 64, bit es 0 del siguiente.
    }

    // Limpiar bloques restantes
    for (size_t i = block + 1; i < n_blocks; ++i) {
      blocks[i] = 0;
    }
  }

  // Check para el orden léctico:
  // Comprueba si this e 'intersection' coinciden en todos los bits ANTES de 'index'
  bool equal_prefix(const BitSet& other, int index) const {
    size_t block = index / 64;
    size_t bit = index % 64;

    for (size_t i = 0; i < block; ++i) {
      if (blocks[i] != other.blocks[i]) return false;
    }

    if (bit > 0) {
      uint64_t mask = (1ULL << bit) - 1;
      if ((blocks[block] & mask) != (other.blocks[block] & mask)) return false;
    }
    return true;
  }

  // Count set bits (Population count)
  int count() const {
    int c = 0;
    for (size_t i = 0; i < n_blocks; ++i) {
      // __builtin_popcountl es específico de GCC/Clang.
      // Para portabilidad Rcpp pura, usamos un loop o bit hacking,
      // pero __builtin es standard en Rtools.
      c += __builtin_popcountll(blocks[i]);
    }
    return c;
  }
};

struct Implication {
  BitSet lhs;
  BitSet rhs;
  Implication(int size) : lhs(size), rhs(size) {}
};

// =============================================================================
// --- CLASE SOLVER ---
// =============================================================================

class BinaryNextClosureSolver {
public:
  int n_obj;
  int n_attr;

  // Contexto (Bitmasks precalculadas)
  // objects_by_attr[j] tiene un bit 1 en la posición i si el objeto i tiene el atributo j
  std::vector<BitSet> objects_by_attr;

  // attributes_by_obj[i] tiene un bit 1 en la posición j si el objeto i tiene el atributo j
  std::vector<BitSet> attributes_by_obj;

  // Base de implicaciones encontrada
  std::vector<Implication> basis;

  // Temporales para evitar allocs en bucles
  BitSet temp_extent;
  BitSet temp_closure;
  BitSet candidate;

  BinaryNextClosureSolver(IntegerMatrix I) :
    n_obj(I.nrow()),
    n_attr(I.ncol()),
    temp_extent(n_obj),
    temp_closure(n_attr),
    candidate(n_attr)
  {
    // 1. Inicializar estructuras del contexto
    objects_by_attr.reserve(n_attr);
    for(int j=0; j<n_attr; ++j) objects_by_attr.emplace_back(n_obj);

    attributes_by_obj.reserve(n_obj);
    for(int i=0; i<n_obj; ++i) attributes_by_obj.emplace_back(n_attr);

    // 2. Cargar datos
    for (int j = 0; j < n_attr; j++) {
      for (int i = 0; i < n_obj; i++) {
        if (I(i, j) == 1) {
          objects_by_attr[j].set(i);
          attributes_by_obj[i].set(j);
        }
      }
    }
  }

  // Calcula el operador de cierre del contexto: A''
  // 1. Extent = intersección de filas de los atributos en A
  // 2. Closure = intersección de columnas de los objetos en Extent
  void compute_context_closure(const BitSet& A, BitSet& out_closure) {
    // --- Paso 1: Calcular Extent (A') ---
    // Empezamos asumiendo todos los objetos (vector de unos)
    temp_extent.clear();
    for(auto& b : temp_extent.blocks) b = ALL_ONES;

    // Intersección
    bool first = true;
    // Optimization: iterate blocks of A
    for (size_t b = 0; b < A.n_blocks; ++b) {
      uint64_t block = A.blocks[b];
      if (block == 0) continue;

      for (int bit = 0; bit < 64; ++bit) {
        if ((block >> bit) & 1ULL) {
          int attr_idx = b * 64 + bit;
          if (attr_idx >= n_attr) break;

          // AND con la columna del atributo
          BitSet& col_mask = objects_by_attr[attr_idx];
          for(size_t k=0; k < temp_extent.n_blocks; ++k) {
            temp_extent.blocks[k] &= col_mask.blocks[k];
          }
        }
      }
    }

    // Ajuste de máscara final para bits sobrantes en extent (si n_obj no es múltiplo de 64)
    if (n_obj % 64 != 0) {
      uint64_t mask = (1ULL << (n_obj % 64)) - 1;
      temp_extent.blocks.back() &= mask;
    }

    // --- Paso 2: Calcular Closure (A'') ---
    // Empezamos asumiendo todos los atributos
    out_closure.clear();
    for(auto& b : out_closure.blocks) b = ALL_ONES;

    // Comprobamos si el extent está vacío
    bool extent_is_empty = true;
    for(auto b : temp_extent.blocks) if(b != 0) { extent_is_empty = false; break; }

    if (extent_is_empty) {
      // Si extent vacío, el closure es todo el conjunto de atributos
      // (Matemáticamente A' = vacío => A'' = M)
      // Ya está seteado a ALL_ONES, solo ajustamos el final
    } else {
      for (size_t b = 0; b < temp_extent.n_blocks; ++b) {
        uint64_t block = temp_extent.blocks[b];
        if (block == 0) continue;

        for (int bit = 0; bit < 64; ++bit) {
          if ((block >> bit) & 1ULL) {
            int obj_idx = b * 64 + bit;
            if (obj_idx >= n_obj) break;

            // AND con la fila del objeto
            BitSet& row_mask = attributes_by_obj[obj_idx];
            for(size_t k=0; k < out_closure.n_blocks; ++k) {
              out_closure.blocks[k] &= row_mask.blocks[k];
            }
          }
        }
      }
    }

    // Ajuste final para atributos
    if (n_attr % 64 != 0) {
      uint64_t mask = (1ULL << (n_attr % 64)) - 1;
      out_closure.blocks.back() &= mask;
    }
  }

  // Cierre Lógico (LinClosure): Cierre bajo las implicaciones encontradas hasta ahora.
  // Algoritmo: Repetir hasta estabilidad: Si LHS \subseteq A, entonces A = A U RHS.
  void lin_closure(BitSet& A) {
    bool changed = true;
    while(changed) {
      changed = false;
      for(const auto& imp : basis) {
        // Si LHS es subconjunto de A
        if (imp.lhs.is_subset_of(A)) {
          // Si RHS NO es subconjunto de A (significa que podemos añadir algo)
          if (!imp.rhs.is_subset_of(A)) {
            A.set_union(imp.rhs);
            changed = true;
          }
        }
      }
    }
  }
};

// =============================================================================
// --- FUNCIÓN EXPORTADA ---
// =============================================================================

// [[Rcpp::export]]
List binary_next_closure_implications(IntegerMatrix I, bool verbose = false) {

  auto start_time = std::chrono::high_resolution_clock::now();

  BinaryNextClosureSolver solver(I);
  int n_attr = solver.n_attr;

  // Conjunto A actual (empieza vacío)
  BitSet A(n_attr);
  // Calcular cierre lógico inicial (probablemente vacío si no hay implicaciones aún)
  solver.lin_closure(A);

  // Contenedores temporales para los resultados para convertirlos a S4 al final
  // Usamos vectores de enteros para construir la matriz sparse
  std::vector<int> lhs_i, lhs_p;
  std::vector<int> rhs_i, rhs_p;
  lhs_p.push_back(0);
  rhs_p.push_back(0);

  if (verbose) Rprintf("Starting Binary NextClosure...\n");

  int count = 0;

  while (true) {
    if (count % 100 == 0) Rcpp::checkUserInterrupt();

    // 1. Calcular cierre del contexto A''
    solver.compute_context_closure(A, solver.temp_closure);

    // 2. Si A != A'', hemos encontrado un pseudo-cerrado (o premisa)
    if (!A.equals(solver.temp_closure)) {
      // Nueva implicación: A -> A'' \ A
      Implication new_imp(n_attr);
      new_imp.lhs.copyFrom(A);

      // RHS = A'' \ A
      new_imp.rhs.set_difference(solver.temp_closure, A);

      solver.basis.push_back(new_imp);

      // Guardar para output R
      int lhs_cnt = 0;
      for(int k=0; k<n_attr; ++k) if(new_imp.lhs.get(k)) { lhs_i.push_back(k); lhs_cnt++; }
      lhs_p.push_back(lhs_p.back() + lhs_cnt);

      int rhs_cnt = 0;
      for(int k=0; k<n_attr; ++k) if(new_imp.rhs.get(k)) { rhs_i.push_back(k); rhs_cnt++; }
      rhs_p.push_back(rhs_p.back() + rhs_cnt);

      count++;
      if (verbose && count % 50 == 0) Rprintf("Found %d implications...\n", count);
    }

    // 3. NextClosure Step (Encontrar el siguiente conjunto pseudo-cerrado en orden léctico)
    bool success = false;

    // Iterar i desde n-1 hasta 0
    for (int i = n_attr - 1; i >= 0; --i) {

      if (A.get(i)) {
        // Si i está en A, lo eliminamos (y tratamos de avanzar)
        A.unset(i);
      } else {
        // Si i NO está en A:
        // Generamos candidato B = (A AND {0..i-1}) U {i}
        // Nota: Debido al bucle, si llegamos aquí, los elementos > i ya han sido borrados de A.
        // Así que B es simplemente A U {i}.

        solver.candidate.copyFrom(A);
        solver.candidate.set(i);

        // Aplicar LinClosure (Cierre bajo las implicaciones actuales)
        solver.lin_closure(solver.candidate);

        // Comprobación Léctica:
        // El nuevo candidato debe preservar el prefijo < i de A.
        // lin_closure puede haber añadido elementos.
        // Si ha añadido algún elemento j < i, entonces no es el siguiente canónico.
        if (solver.candidate.equal_prefix(A, i)) {
          A.copyFrom(solver.candidate);
          success = true;
          break;
        }
      }
    }

    if (!success) break; // No hay más conjuntos
  }

  // --- Construcción de Objetos de Salida (S4 dgCMatrix) ---

  int n_imps = solver.basis.size();

  S4 LHS_S4("dgCMatrix");
  LHS_S4.slot("i") = wrap(lhs_i);
  LHS_S4.slot("p") = wrap(lhs_p);
  LHS_S4.slot("x") = NumericVector(lhs_i.size(), 1.0);
  LHS_S4.slot("Dim") = IntegerVector::create(n_attr, n_imps);

  S4 RHS_S4("dgCMatrix");
  RHS_S4.slot("i") = wrap(rhs_i);
  RHS_S4.slot("p") = wrap(rhs_p);
  RHS_S4.slot("x") = NumericVector(rhs_i.size(), 1.0);
  RHS_S4.slot("Dim") = IntegerVector::create(n_attr, n_imps);

  auto end_time = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed = end_time - start_time;

  if (verbose) Rprintf("Finished. Total implications: %d. Time: %.4fs\n", n_imps, elapsed.count());

  return List::create(
    _["LHS"] = LHS_S4,
    _["RHS"] = RHS_S4,
    _["elapsed"] = elapsed.count()
  );
}
