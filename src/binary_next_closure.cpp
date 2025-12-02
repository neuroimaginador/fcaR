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

  // Check para el orden léctico
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
  std::vector<BitSet> objects_by_attr;
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
  void compute_context_closure(const BitSet& A, BitSet& out_closure) {
    // --- Paso 1: Calcular Extent (A') ---
    temp_extent.clear();
    for(auto& b : temp_extent.blocks) b = ALL_ONES;

    for (size_t b = 0; b < A.n_blocks; ++b) {
      uint64_t block = A.blocks[b];
      if (block == 0) continue;

      for (int bit = 0; bit < 64; ++bit) {
        if ((block >> bit) & 1ULL) {
          int attr_idx = b * 64 + bit;
          if (attr_idx >= n_attr) break;

          BitSet& col_mask = objects_by_attr[attr_idx];
          for(size_t k=0; k < temp_extent.n_blocks; ++k) {
            temp_extent.blocks[k] &= col_mask.blocks[k];
          }
        }
      }
    }

    if (n_obj % 64 != 0) {
      uint64_t mask = (1ULL << (n_obj % 64)) - 1;
      temp_extent.blocks.back() &= mask;
    }

    // --- Paso 2: Calcular Closure (A'') ---
    out_closure.clear();
    for(auto& b : out_closure.blocks) b = ALL_ONES;

    bool extent_is_empty = true;
    for(auto b : temp_extent.blocks) if(b != 0) { extent_is_empty = false; break; }

    if (!extent_is_empty) {
      for (size_t b = 0; b < temp_extent.n_blocks; ++b) {
        uint64_t block = temp_extent.blocks[b];
        if (block == 0) continue;

        for (int bit = 0; bit < 64; ++bit) {
          if ((block >> bit) & 1ULL) {
            int obj_idx = b * 64 + bit;
            if (obj_idx >= n_obj) break;

            BitSet& row_mask = attributes_by_obj[obj_idx];
            for(size_t k=0; k < out_closure.n_blocks; ++k) {
              out_closure.blocks[k] &= row_mask.blocks[k];
            }
          }
        }
      }
    }

    if (n_attr % 64 != 0) {
      uint64_t mask = (1ULL << (n_attr % 64)) - 1;
      out_closure.blocks.back() &= mask;
    }
  }

  // Cierre Lógico (LinClosure)
  void lin_closure(BitSet& A) {
    bool changed = true;
    while(changed) {
      changed = false;
      for(const auto& imp : basis) {
        if (imp.lhs.is_subset_of(A)) {
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
  int n_obj = solver.n_obj;

  // Conjunto A actual (empieza vacío)
  BitSet A(n_attr);
  solver.lin_closure(A);

  // Contenedores para Implicaciones (LHS -> RHS)
  std::vector<int> lhs_i, lhs_p;
  std::vector<int> rhs_i, rhs_p;
  lhs_p.push_back(0);
  rhs_p.push_back(0);

  // Contenedores para Conceptos (Extents x Intents)
  std::vector<int> int_i, int_p;
  std::vector<int> ext_i, ext_p;
  int_p.push_back(0);
  ext_p.push_back(0);

  if (verbose) Rprintf("Starting Binary NextClosure (Concepts + Implications)...\n");

  int count_imp = 0;
  int count_concepts = 0;

  while (true) {
    if (count_concepts % 100 == 0) Rcpp::checkUserInterrupt();

    // 1. Calcular cierre del contexto A''
    // Nota: temp_extent tendrá A' (Extent) y temp_closure tendrá A'' (Intent)
    solver.compute_context_closure(A, solver.temp_closure);

    // 2. Clasificar A: ¿Es un concepto cerrado o un pseudo-cerrado?
    if (A.equals(solver.temp_closure)) {
      // --- ES UN CONCEPTO (A == A'') ---

      // Guardar Intent (A)
      int int_cnt = 0;
      for(int k=0; k<n_attr; ++k) if(A.get(k)) { int_i.push_back(k); int_cnt++; }
      int_p.push_back(int_p.back() + int_cnt);

      // Guardar Extent (A', que está en temp_extent)
      int ext_cnt = 0;
      for(int k=0; k<n_obj; ++k) if(solver.temp_extent.get(k)) { ext_i.push_back(k); ext_cnt++; }
      ext_p.push_back(ext_p.back() + ext_cnt);

      count_concepts++;

    } else {
      // --- ES UN PSEUDO-CERRADO (A != A'') ---

      // Nueva implicación: A -> A'' \ A
      Implication new_imp(n_attr);
      new_imp.lhs.copyFrom(A);
      new_imp.rhs.set_difference(solver.temp_closure, A);

      solver.basis.push_back(new_imp);

      // Guardar implicación para output R
      int lhs_cnt = 0;
      for(int k=0; k<n_attr; ++k) if(new_imp.lhs.get(k)) { lhs_i.push_back(k); lhs_cnt++; }
      lhs_p.push_back(lhs_p.back() + lhs_cnt);

      int rhs_cnt = 0;
      for(int k=0; k<n_attr; ++k) if(new_imp.rhs.get(k)) { rhs_i.push_back(k); rhs_cnt++; }
      rhs_p.push_back(rhs_p.back() + rhs_cnt);

      count_imp++;
      if (verbose && count_imp % 50 == 0) Rprintf("Found %d implications...\n", count_imp);
    }

    // 3. NextClosure Step
    bool success = false;

    // Iterar i desde n-1 hasta 0
    for (int i = n_attr - 1; i >= 0; --i) {

      if (A.get(i)) {
        A.unset(i);
      } else {
        solver.candidate.copyFrom(A);
        solver.candidate.set(i);

        // Aplicar LinClosure (Cierre bajo las implicaciones actuales)
        solver.lin_closure(solver.candidate);

        if (solver.candidate.equal_prefix(A, i)) {
          A.copyFrom(solver.candidate);
          success = true;
          break;
        }
      }
    }

    if (!success) break;
  }

  // --- Construcción de Objetos de Salida (S4 dgCMatrix) ---

  // 1. Implicaciones
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

  // 2. Conceptos
  // count_concepts es el número total de conceptos encontrados

  S4 Intents_S4("dgCMatrix");
  Intents_S4.slot("i") = wrap(int_i);
  Intents_S4.slot("p") = wrap(int_p);
  Intents_S4.slot("x") = NumericVector(int_i.size(), 1.0);
  Intents_S4.slot("Dim") = IntegerVector::create(n_attr, count_concepts);

  S4 Extents_S4("dgCMatrix");
  Extents_S4.slot("i") = wrap(ext_i);
  Extents_S4.slot("p") = wrap(ext_p);
  Extents_S4.slot("x") = NumericVector(ext_i.size(), 1.0);
  Extents_S4.slot("Dim") = IntegerVector::create(n_obj, count_concepts);

  auto end_time = std::chrono::high_resolution_clock::now();
  std::chrono::duration<double> elapsed = end_time - start_time;

  if (verbose) {
    Rprintf("Finished.\n");
    Rprintf("  Total concepts: %d\n", count_concepts);
    Rprintf("  Total implications: %d\n", n_imps);
    Rprintf("  Time: %.4fs\n", elapsed.count());
  }

  return List::create(
    _["concepts"] = Intents_S4,
    _["extents"] = Extents_S4,
    _["LHS"] = LHS_S4,
    _["RHS"] = RHS_S4,
    _["elapsed"] = elapsed.count()
  );
}
