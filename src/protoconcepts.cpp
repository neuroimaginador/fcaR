#include <Rcpp.h>
#include "set_operations_galois.h"
#include "vector_operations.h"
#include <map>
#include <vector>
#include <iostream>

using namespace Rcpp;

// Wrapper for SparseVector to use as key in std::map and handle memory
class SparseVectorWrapper {
public:
  SparseVector vec;

  SparseVectorWrapper() {
    initVector(&vec, 0);
  }

  SparseVectorWrapper(SparseVector v) {
    initVector(&vec, v.length);
    cloneVector(&vec, v);
    // cloneVector doesn't copy p, so do it manually
    if (v.p.used > 0) {
       for (size_t k = 0; k < v.p.used; ++k) {
         insertArray(&(vec.p), v.p.array[k]);
       }
    }
  }

  SparseVectorWrapper(const SparseVectorWrapper& other) {
    initVector(&vec, other.vec.length);
    cloneVector(&vec, other.vec);
    // cloneVector doesn't copy p, so do it manually
    if (other.vec.p.used > 0) {
       for (size_t k = 0; k < other.vec.p.used; ++k) {
         insertArray(&(vec.p), other.vec.p.array[k]);
       }
    }
  }

  SparseVectorWrapper& operator=(const SparseVectorWrapper& other) {
    if (this != &other) {
      freeVector(&vec);
      initVector(&vec, other.vec.length);
      cloneVector(&vec, other.vec);
      // cloneVector doesn't copy p, so do it manually
      if (other.vec.p.used > 0) {
         for (size_t k = 0; k < other.vec.p.used; ++k) {
           insertArray(&(vec.p), other.vec.p.array[k]);
         }
      }
    }
    return *this;
  }

  ~SparseVectorWrapper() {
    freeVector(&vec);
  }

  // Strict weak ordering for std::map
  bool operator<(const SparseVectorWrapper& other) const {
    if (vec.length != other.vec.length) return vec.length < other.vec.length;
    if (vec.i.used != other.vec.i.used) return vec.i.used < other.vec.i.used;
    
    for (size_t k = 0; k < vec.i.used; ++k) {
      if (vec.i.array[k] != other.vec.i.array[k]) {
        return vec.i.array[k] < other.vec.i.array[k];
      }
      // Compare values for fuzzy sets
      if (std::abs(vec.x.array[k] - other.vec.x.array[k]) > 1e-9) {
          return vec.x.array[k] < other.vec.x.array[k];
      }
    }
    return false;
  }
};

// Helper to convert integer subset to SparseVector (crisp)
SparseVector subset_to_sparse(unsigned long long subset_mask, int size) {
  SparseVector v;
  initVector(&v, size);
  for (int i = 0; i < size; ++i) {
    if ((subset_mask >> i) & 1) {
      insertArray(&(v.i), i);
      insertArray(&(v.x), 1.0);
    }
  }
  // Construct 'p' array as expected by some functions
  insertArray(&(v.p), 0);
  insertArray(&(v.p), v.i.used);
  return v;
}

// [[Rcpp::export]]
List find_protoconcepts_cpp(NumericMatrix I, String connection = "standard", String name = "Zadeh", bool verbose = false) {
  
  GaloisOperator intent_f = get_intent_function(connection);
  GaloisOperator extent_f = get_extent_function(connection);
  LogicOperator tnorm = get_tnorm(name);
  LogicOperator implication = get_implication(name);

  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  // Maps to store equivalence classes
  // Key: Resulting Attribute Set (SparseVectorWrapper)
  // Value: List of Source Sets (List of SparseVectorWrapper)
  std::map<SparseVectorWrapper, std::vector<SparseVectorWrapper>> obj_equiv_classes;
  std::map<SparseVectorWrapper, std::vector<SparseVectorWrapper>> att_equiv_classes;

  if (verbose) Rcout << "Processing object subsets..." << std::endl;

  // 1. Iterate all subsets of objects A
  unsigned long long n_obj_subsets = 1ULL << n_objects;
  for (unsigned long long mask = 0; mask < n_obj_subsets; ++mask) {
    if (mask % 1000 == 0) Rcpp::checkUserInterrupt();

    SparseVector A = subset_to_sparse(mask, n_objects);
    
    SparseVector O; // A' (intent)
    initVector(&O, n_attributes);
    // Use intent_f to compute derivative
    intent_f(&O, A, I.begin(), n_objects, n_attributes, tnorm, implication);

    SparseVectorWrapper key(O);
    SparseVectorWrapper val(A);
    
    obj_equiv_classes[key].push_back(val);
    
    freeVector(&A);
    freeVector(&O);
  }

  if (verbose) Rcout << "Processing attribute subsets..." << std::endl;

  // 2. Iterate all subsets of attributes B
  unsigned long long n_att_subsets = 1ULL << n_attributes;
  for (unsigned long long mask = 0; mask < n_att_subsets; ++mask) {
    if (mask % 1000 == 0) Rcpp::checkUserInterrupt();

    SparseVector B = subset_to_sparse(mask, n_attributes);
    
    SparseVector P; // B'' (closure)
    initVector(&P, n_attributes);
    
    // Compute closure: B -> B' (extent) -> (B')' (intent)
    SparseVector temp_extent;
    initVector(&temp_extent, n_objects);
    
    extent_f(&temp_extent, B, I.begin(), n_objects, n_attributes, tnorm, implication);
    intent_f(&P, temp_extent, I.begin(), n_objects, n_attributes, tnorm, implication);
    
    SparseVectorWrapper key(P);
    SparseVectorWrapper val(B);
    
    att_equiv_classes[key].push_back(val);
    
    freeVector(&B);
    freeVector(&P);
    freeVector(&temp_extent);
  }

  if (verbose) Rcout << "Matching equivalence classes..." << std::endl;

  // 3. Find intersections and build result
  // List of concepts: each element is a list of two S4 matrices (objects, attributes)
  // Or better: return two lists of S4 matrices? Or one list of pairs?
  // Let's return a List of Lists where each inner list is (A, B)
  
  List results;
  
  for (auto const& pair_item : obj_equiv_classes) {
    const SparseVectorWrapper& key = pair_item.first;
    const std::vector<SparseVectorWrapper>& obj_list = pair_item.second;
    if (att_equiv_classes.count(key)) {
      const std::vector<SparseVectorWrapper>& att_list = att_equiv_classes[key];
      
      // key is the common attribute set (A' == B'')
      // We need to pair every A in obj_list with every B in att_list
      
      for (const auto& A : obj_list) {
        for (const auto& B : att_list) {
           // Create pair
           List pair = List::create(
             SparseToS4_fast(A.vec),
             SparseToS4_fast(B.vec)
           );
           results.push_back(pair);
        }
      }
    }
  }

  return results;
}
