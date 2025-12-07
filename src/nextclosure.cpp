#include "Logics.h"
#include "binary_operations.h"
#include "set_operations_galois.h"
#include <Rcpp.h>
#include <chrono>

using namespace Rcpp;

static void chkIntFn(void *dummy) { R_CheckUserInterrupt(); }

// this will call the above in a top-level context so it won't longjmp-out of
// your context
bool checkInterrupt() { return (R_ToplevelExec(chkIntFn, NULL) == FALSE); }

// Ganter's Next Closure Algorithm
// This algorithm computes all formal concepts (or closed sets) of a formal
// context. It generates them in the "lectic" order (a lexicographical order on
// bitsets).
//
// References:
// Ganter, B. (1984). Two basic algorithms in concept analysis.

// Functions to compute the next pseudo-closed set

// Computes the "direct sum" $A \oplus i$ in the context of the Next Closure
// algorithm. It finds the smallest set strictly greater than A in the lectic
// order at index i.
// Specifically, $A \oplus i$ is defined as:
// $((A \cap \{1, \dots, i-1\}) \cup \{i\})$
// But here, since we deal with fuzzy grades or multiple values, the logic is
// slightly more complex to handle the "grade" at index i.
bool compute_direct_sum(SparseVector A, int a_i, double grade_i, int imax,
                        SparseVector *res) {

  reinitVector(res);

  cloneVector(res, A);
  bool can = true;

  int resp = res->i.used;

  for (size_t i = 0; i < A.i.used; i++) {

    if (A.i.array[i] == a_i) {

      if ((A.x.array[i] - grade_i) >= -1.e-3) {

        // Rcout << "    -> NO: " << A.x.array[i] << " >= " << grade_i << "\n";

        can = false;

      } else {

        // Rcout << "    -> SI: " << A.x.array[i] << " < " << grade_i << "\n";
      }
    }

    if (A.i.array[i] >= a_i) {

      resp = i;
      break;
    }
  }

  assignUsed(&(res->i), resp);
  assignUsed(&(res->x), resp);

  insertArray(&(res->i), a_i);
  insertArray(&(res->x), grade_i);

  return can;
}

bool binary_compute_direct_sum(BinarySparseVector A, int a_i, int imax,
                               BinarySparseVector *res) {
  reinitArray(&(res->i));
  for (size_t i = 0; i < A.i.used; ++i) {
    if (A.i.array[i] < a_i) {
      insertArray(&(res->i), A.i.array[i]);
    }
  }
  insertArray(&(res->i), a_i);
  return true; // In binary case, we can always add an attribute
}

void semantic_closure(SparseVector A, ImplicationTree t, SparseVector LHS,
                      SparseVector RHS, SparseVector *res) {

  int n_attributes = A.length;

  reinitVector(res);

  cloneVector(res, A);

  if (RHS.p.used != 0) {

    int n = RHS.p.used;

    SparseVector res2;
    SparseVector res3;

    initVector(&res2, n_attributes);
    initVector(&res3, n_attributes);

    IntArray subsets;
    initArray(&subsets, n);
    bool *black_list = (bool *)malloc(n * sizeof(bool));

    for (int i = 0; i < n; i++) {

      black_list[i] = true;
    }

    is_subset(A, t, &subsets, black_list);

    while (subsets.used > 0) {

      setunion(RHS, subsets, &res2);

      setunion2(*res, res2, &res3);

      cloneVector(res, res3);

      reinitVector(&res2);
      reinitVector(&res3);

      for (size_t i = 0; i < subsets.used; i++) {

        black_list[subsets.array[i]] = false;
      }

      is_subset(*res, t, &subsets, black_list);
    }

    freeVector(&res2);
    freeVector(&res3);

    freeArray(&subsets);

    free(black_list);
  }
}

// Checks if the candidate set B is the "next" closed set after A in the lectic
// order.
// A set B comes lexically after A if at the first index i where they differ,
// B contains i (or a larger grade) and A does not.
// For the Next Closure algorithm, we generate candidates $C = (A \oplus i)''$
// (closure of direct sum). We must verify that $C$ is the *canonical* next
// concept. This is true if $C$ preserves the prefix of $A$ up to $i$. i.e., $C
// \cap \{1, \dots, i-1\} = A \cap \{1, \dots, i-1\}$. If $C$ adds any element
// $j < i$ that was not in $A$, then $C$ should have been generated when
// processing index $j$, so we reject it now (it's not the *next* one, or it was
// already found).
bool is_set_preceding2(SparseVector B, SparseVector C, int a_i,
                       double grade_i) {

  if (grade_i == 0 || grade_i == -1) {
    return false;
  }

  size_t bi_lt_a_i_size = 0, ci_lt_a_i_size = 0;
  size_t bi_idx = 0, ci_idx = 0;
  double bx_at_a_i = 0.0, cx_at_a_i = 0.0;

  // Find elements in B and C that are less than a_i
  for (size_t i = 0; i < B.i.used && bi_idx < B.i.used; i++) {
    if (B.i.array[i] < a_i) {
      bi_lt_a_i_size++;
    } else if (B.i.array[i] == a_i) {
      bx_at_a_i = B.x.array[i];
    } else {
      break;
    }
    bi_idx++;
  }

  for (size_t i = 0; i < C.i.used && ci_idx < C.i.used; i++) {
    if (C.i.array[i] < a_i) {
      ci_lt_a_i_size++;
    } else if (C.i.array[i] == a_i) {
      cx_at_a_i = C.x.array[i];
    } else {
      break;
    }
    ci_idx++;
  }

  if (cx_at_a_i != grade_i) {
    return false;
  }

  if (grade_i == 1 && bx_at_a_i == 1) {
    return false;
  }

  if (grade_i == -1 && bx_at_a_i != -1) {
    return false;
  }

  if (ci_lt_a_i_size != bi_lt_a_i_size) {
    return false;
  }

  // Check that the elements less than a_i in B and C are the same
  // This enforces the lectic order condition: the prefix must match.
  for (size_t i = 0; i < ci_lt_a_i_size; i++) {
    if (C.i.array[i] != B.i.array[i] || C.x.array[i] != B.x.array[i]) {
      return false;
    }
  }

  return true;
}

bool is_set_preceding(SparseVector B, SparseVector C, int a_i, double grade_i) {

  // Rprintf("Comparing:\n");

  IntArray bi_lt_a_i, ci_lt_a_i;
  DoubleArray bx_lt_a_i, cx_lt_a_i;

  initArray(&bi_lt_a_i, B.length);
  initArray(&ci_lt_a_i, C.length);
  initArray(&bx_lt_a_i, B.length);
  initArray(&cx_lt_a_i, C.length);

  double bx_at_a_i = 0.0, cx_at_a_i = 0.0;
  for (size_t i = 0; i < B.i.used; i++) {

    if (B.i.array[i] < a_i) {

      insertArray(&bi_lt_a_i, B.i.array[i]);
      insertArray(&bx_lt_a_i, B.x.array[i]);
    }

    if (B.i.array[i] == a_i) {

      bx_at_a_i = B.x.array[i];
    }
  }

  for (size_t i = 0; i < C.i.used; i++) {

    if (C.i.array[i] < a_i) {

      insertArray(&ci_lt_a_i, C.i.array[i]);
      insertArray(&cx_lt_a_i, C.x.array[i]);
    }

    if (C.i.array[i] == a_i) {

      cx_at_a_i = C.x.array[i];
    }
  }

  if (fabs(cx_at_a_i - grade_i) > 1.e-3) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    // Rcout << "  -> Reason 1: " << cx_at_a_i << " != " << grade_i << "\n";

    return false;
  }

  if (bx_at_a_i >= cx_at_a_i) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    // Rcout << "  -> Reason 2\n";
    return false;
  }

  if (ci_lt_a_i.used != bi_lt_a_i.used) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    // Rcout << "  -> Reason 3\n";
    return false;
  }

  for (size_t i = 0; i < ci_lt_a_i.used; i++) {

    if (ci_lt_a_i.array[i] != bi_lt_a_i.array[i]) {

      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);

      // Rcout << "  -> Reason 4\n";
      return false;
    }
    if (fabs(cx_lt_a_i.array[i] - bx_lt_a_i.array[i]) > 1.e-3) {

      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);

      // Rcout << "  -> Reason 5\n";
      return false;
    }
  }

  freeArray(&cx_lt_a_i);
  freeArray(&bx_lt_a_i);
  freeArray(&ci_lt_a_i);
  freeArray(&bi_lt_a_i);

  return true;
}

bool binary_is_set_preceding(const BinarySparseVector &B,
                             const BinarySparseVector &C, int a_i) {
  size_t b_idx = 0;
  size_t c_idx = 0;

  // Check if C restricted to elements smaller than a_i is the same as B
  while (b_idx < B.i.used && B.i.array[b_idx] < a_i && c_idx < C.i.used &&
         C.i.array[c_idx] < a_i) {
    if (B.i.array[b_idx] != C.i.array[c_idx]) {
      return false;
    }
    b_idx++;
    c_idx++;
  }

  // Check if there are remaining elements in B or C that are smaller than a_i
  if ((b_idx < B.i.used && B.i.array[b_idx] < a_i) ||
      (c_idx < C.i.used && C.i.array[c_idx] < a_i)) {
    return false;
  }

  // Check if a_i is in C but not in B
  bool c_has_ai = false;
  while (c_idx < C.i.used) {
    if (C.i.array[c_idx] == a_i) {
      c_has_ai = true;
      break;
    }
    c_idx++;
  }

  if (!c_has_ai)
    return false;

  while (b_idx < B.i.used) {
    if (B.i.array[b_idx] == a_i)
      return false;
    b_idx++;
  }

  return true;
}

// Main loop to find the next closed set A+.
// It searches for the largest index $i$ such that $A \oplus i$ is a valid
// generator for the next concept.
void compute_next_closure(SparseVector A, int i, int imax,
                          ListOf<NumericVector> grades_set, ImplicationTree t,
                          SparseVector LHS, SparseVector RHS,
                          StringVector attrs, SparseVector *candB) {

  // SparseVector candB;
  // initVector(&candB, A.length);

  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);

  bool exit = false;

  for (int a_i = i - 1; a_i >= 0; a_i--) {

    n_grades = grades_set[a_i].size();

    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {

      bool can =
          compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      if (!can)
        continue;

      semantic_closure(*candB, t, LHS, RHS, &candB2);

      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {

        cloneVector(candB, candB2);
        freeVector(&candB2);

        exit = true;

        // return candB;
      }

      if (exit)
        break;
    }

    if (exit)
      break;
  }

  // Rprintf("Something went wrong...\n");
  //
  // return candB;
}

// [[Rcpp::export]]
List next_closure_implications(NumericMatrix I, List grades_set,
                               StringVector attrs,
                               String connection = "standard",
                               String name = "Zadeh", bool save_concepts = true,
                               bool verbose = false) {

  auto start = std::chrono::high_resolution_clock::now(); // Marca de inicio

  LogicOperator implication = get_implication(name);
  LogicOperator tnorm = get_tnorm(name);
  GaloisOperator intent_f = get_intent_function(connection);
  GaloisOperator extent_f = get_extent_function(connection);

  int n_attributes = attrs.size();
  int n_objects = I.nrow();

  int n_imp = 0;

  SparseVector concepts;
  SparseVector extents;
  initVector(&concepts, n_attributes);
  initVector(&extents, I.nrow());

  SparseVector LHS, RHS;
  initVector(&LHS, n_attributes);
  initVector(&RHS, n_attributes);

  SparseVector empty, B, rhs;

  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);
  initVector(&rhs, n_attributes);

  ImplicationTree tree;
  // (ImplicationTree*)malloc(sizeof(ImplicationTree));
  initImplicationTree(&tree, n_attributes);

  SparseVector A;
  initVector(&A, n_attributes);
  compute_closure(&A, empty, I.begin(), n_objects, n_attributes, extent_f,
                  intent_f, tnorm, implication);

  SparseVector this_extent;
  initVector(&this_extent, n_objects);

  if (cardinal(A) > 0) {

    add_column(&LHS, empty);
    add_column(&RHS, A);
    addImplicationToTree(&tree, empty);

    if (verbose) {

      Rcout << "Added initial implication to basis" << std::endl
            << std::endl
            << std::endl;
      // printVector(A, attrs);
      // Rcout << std::endl << std::endl;
      // printImpl(empty, A, attrs);

      n_imp++;
    }
  }

  if (save_concepts) {

    reinitVector(&this_extent);
    extent_f(&this_extent, A, I.begin(), n_objects, n_attributes, tnorm,
             implication);
    add_column(&concepts, A);
    add_column(&extents, this_extent);
  }

  if (verbose & save_concepts) {

    Rprintf("Added concept:\n");

    if (cardinal(A) > 0) {

      printVector(A, attrs);

    } else {

      Rprintf("{}");
    }

    Rprintf("\n");
  }

  int count = 0;

  double pctg, old_pctg = 0;

  while ((cardinal(A) < n_attributes)) {

    compute_next_closure(A, n_attributes, n_attributes, grades_set, tree, LHS,
                         RHS, attrs, &B);

    cloneVector(&A, B);

    reinitVector(&B);
    compute_closure(&B, A, I.begin(), n_objects, n_attributes, extent_f,
                    intent_f, tnorm, implication);

    setdifference(B, A, &rhs);

    if (cardinal(rhs) == 0) {

      // Concept
      if (save_concepts) {

        reinitVector(&this_extent);
        extent_f(&this_extent, A, I.begin(), n_objects, n_attributes, tnorm,
                 implication);

        add_column(&concepts, A);
        add_column(&extents, this_extent);

        if (verbose) {

          Rprintf("Added concept:\n");
          // printVector(A, attrs);
          // Rprintf("\n");
        }
      }

    } else {

      add_column(&LHS, A);
      add_column(&RHS, rhs);

      if (verbose) {

        // Rcout << "Added implication " << n_imp++ << " to basis" << std::endl;
        // printImpl(A, rhs, attrs);
      }

      addImplicationToTree(&tree, A);

      count++;

      if (verbose) {

        if (count % 10 == 0)
          Rprintf("%u\n", count);
      }
    }

    if (checkInterrupt()) { // user interrupted ...

      freeVector(&A);
      freeVector(&empty);
      freeVector(&B);
      freeVector(&rhs);
      freeVector(&this_extent);

      S4 intents_S4 = SparseToS4_fast(concepts);
      S4 extents_S4 = SparseToS4_fast(extents);
      S4 lhs_S4 = SparseToS4_fast(LHS);
      S4 rhs_S4 = SparseToS4_fast(RHS);

      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&LHS);
      freeVector(&RHS);
      freeImplicationTree(&tree);

      auto end = std::chrono::high_resolution_clock::now(); // Marca de fin

      // Calcula la duración en segundos
      std::chrono::duration<double> elapsed = end - start;

      List res = List::create(
          _["concepts"] = intents_S4, _["extents"] = extents_S4,
          _["LHS"] = lhs_S4, _["RHS"] = rhs_S4, _["elapsed"] = elapsed.count());

      Rprintf("User interrupted.\n");
      return res;
    }
  }

  S4 intents_S4 = SparseToS4_fast(concepts);
  S4 extents_S4 = SparseToS4_fast(extents);
  S4 lhs_S4 = SparseToS4_fast(LHS);
  S4 rhs_S4 = SparseToS4_fast(RHS);

  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&LHS);
  freeVector(&RHS);

  auto end = std::chrono::high_resolution_clock::now(); // Marca de fin

  // Calcula la duración en segundos
  std::chrono::duration<double> elapsed = end - start;

  List res = List::create(_["concepts"] = intents_S4, _["extents"] = extents_S4,
                          _["LHS"] = lhs_S4, _["RHS"] = rhs_S4,
                          _["elapsed"] = elapsed.count());

  if (verbose)
    Rprintf("Finished.\n");

  freeVector(&A);
  freeVector(&empty);
  freeVector(&B);
  freeVector(&rhs);
  freeVector(&this_extent);
  freeImplicationTree(&tree);

  return res;
}

// SparseVector compute_next_intent(SparseVector A,
//                                  NumericMatrix I,
//                                  int i,
//                                  int imax,
//                                  ListOf<NumericVector> grades_set,
//                                  int* closure_count) {
//
//
//   SparseVector candB;
//   initVector(&candB, A.length);
//
//   int n_grades = grades_set.size();
//   SparseVector candB2;
//   initVector(&candB2, A.length);
//
//   for (int a_i = i - 1; a_i >= 0; a_i--) {
//
//     n_grades = grades_set[a_i].size();
//
//     for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {
//
//       compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, &candB);
//
//       candB2 = compute_closure(candB, I);
//       cloneVector(&candB, candB2);
//       freeVector(&candB2);
//       (*closure_count)++;
//
//       if (is_set_preceding(A, candB, a_i, grades_set[a_i][grade_idx])) {
//
//         return candB;
//
//       }
//
//     }
//
//   }
//
//   Rprintf("Something went wrong...\n");
//
//   return candB;
//
// }

void compute_next_intent(SparseVector *candB, SparseVector A, NumericMatrix I,
                         int i, int imax, ListOf<NumericVector> grades_set,
                         double *closure_count, StringVector attrs,
                         GaloisOperator extent_f, GaloisOperator intent_f,
                         LogicOperator tnorm, LogicOperator implication,
                         bool verbose = false) {

  // SparseVector candB;
  // initVector(&candB, A.length);
  int n_objects = I.nrow();
  int n_attributes = I.ncol();

  // Rcout << "** Compute next intent" << std::endl;

  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);

  for (int a_i = i - 1; a_i >= 0; a_i--) {

    n_grades = grades_set[a_i].size();

    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {

      bool can =
          compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);
      if (!can)
        continue;
      // Rcout << "candB" << std::endl;
      // printArray(candB->i);
      // printArray(candB->x);
      if (verbose) {

        Rcout << "-> Testing: ";
        printVector(*candB, attrs);
        Rcout << "\n";
      }

      reinitVector(&candB2);
      compute_closure(&candB2, *candB, I.begin(), n_objects, n_attributes,
                      extent_f, intent_f, tnorm, implication);

      if (verbose) {

        Rcout << "-> Its closure is: ";
        printVector(candB2, attrs);
        Rcout << "\n";
      }

      (*closure_count) = (*closure_count) + 1;

      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {

        if (verbose) {

          Rcout << "-> It is valid!\n";
        }
        // return candB;
        cloneVector(candB, candB2);
        freeVector(&candB2);
        return;

      } else {

        if (verbose) {

          Rcout << "-> It is NOT valid!: A = ";
          printVector(A, attrs);
          Rcout << ", a_i = " << a_i << ", g = " << grades_set[a_i][grade_idx]
                << "\n";
        }
      }
    }
  }

  // Rprintf("Something went wrong...\n");
  //
  // return candB;
}

// [[Rcpp::export]]
List next_closure_concepts(NumericMatrix I, ListOf<NumericVector> grades_set,
                           StringVector attrs, String connection = "standard",
                           String name = "Zadeh", bool verbose = false,
                           bool ret = true) {

  int n_objects = I.nrow();
  int n_attributes = attrs.size();
  int n_grades = grades_set[0].size();

  double closure_count = 0.0;

  // Rcout << "Entramos" << std::endl;

  LogicOperator implication = get_implication(name);
  LogicOperator tnorm = get_tnorm(name);
  GaloisOperator intent_f = get_intent_function(connection);
  GaloisOperator extent_f = get_extent_function(connection);

  SparseVector concepts;
  SparseVector extents;
  initMatrix(&concepts, n_attributes);
  initMatrix(&extents, I.nrow());

  SparseVector empty, B;

  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);

  SparseVector A;
  initVector(&A, n_attributes);
  compute_closure(&A, empty, I.begin(), n_objects, n_attributes, extent_f,
                  intent_f, tnorm, implication);
  SparseVector A2;
  initVector(&A2, n_attributes);

  closure_count = closure_count + 1;

  extent_f(&B, A, I.begin(), n_objects, n_attributes, tnorm, implication);
  add_column(&concepts, A);
  add_column(&extents, B);

  if (verbose) {

    Rprintf("Added initial concept:\n");

    if (cardinal(A) > 0) {

      printVector(A, attrs);

    } else {

      Rprintf("{}");
    }

    Rprintf("\n");
  }

  // double pctg, old_pctg = 0;

  while ((cardinal(A) < n_attributes)) {

    // Rcout << "Starting iterations: " << std::endl;
    reinitVector(&A2);
    reinitVector(&B);
    compute_next_intent(&A2, A, I, n_attributes, n_attributes, grades_set,
                        &closure_count, attrs, extent_f, intent_f, tnorm,
                        implication, verbose);

    // A2 = compute_next_intent(A, I,
    //                          n_attributes,
    //                          n_attributes,
    //                          grades_set,
    //                          &closure_count);

    // if (verbose) {
    //
    //   pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;
    //
    //   if (pctg != old_pctg) {
    //
    //     Rprintf("Completed = %.2f\n %", pctg);
    //     old_pctg = pctg;
    //
    //   }
    //
    // }

    // Concept
    add_column(&concepts, A2);
    extent_f(&B, A2, I.begin(), n_objects, n_attributes, tnorm, implication);
    // B = compute_extent(A2, I);
    add_column(&extents, B);

    if (verbose) {

      Rprintf("Added concept:\n");
      // Rcout << A2.i.used << std::endl;
      // printArray(A2.i);
      // printArray(A2.x);
      printVector(A2, attrs);
      Rprintf("\n");
    }

    if (checkInterrupt()) { // user interrupted ...

      S4 intents_S4 = SparseToS4_fast(concepts);
      S4 extents_S4 = SparseToS4_fast(extents);

      freeVector(&A);
      freeVector(&B);
      freeVector(&empty);
      freeVector(&concepts);
      freeVector(&extents);
      freeVector(&A2);

      List res = List::create(
          _["intents"] = intents_S4, _["extents"] = extents_S4,
          _["closure_count"] = closure_count / (double)(n_grades - 1));

      Rprintf("User interrupted.\n");
      return res;
    }

    cloneVector(&A, A2);
    // freeVector(&A2);
  }

  // Rcout << " Number of closures: " << closure_count << std::endl;

  List res;

  if (ret) {

    S4 intents_S4 = SparseToS4_fast(concepts);
    S4 extents_S4 = SparseToS4_fast(extents);

    res = List::create(_["intents"] = intents_S4, _["extents"] = extents_S4,
                       _["closure_count"] =
                           closure_count / (double)(n_grades - 1));

  } else {

    res = List::create(_["closure_count"] =
                           closure_count / (double)(n_grades - 1));
  }

  freeVector(&A);
  freeVector(&B);
  freeVector(&empty);
  freeVector(&concepts);
  freeVector(&extents);
  freeVector(&A2);

  if (verbose)
    Rprintf("Finished.\n");

  return res;
}

// [[Rcpp::export]]
List binary_next_closure_concepts(IntegerMatrix I, bool verbose = false) {

  int n_attributes = I.ncol();

  BinarySparseVector A;
  initVector(&A, n_attributes);

  BinarySparseVector empty;
  initVector(&empty, n_attributes);

  binary_compute_closure(&A, &empty, I);

  List concepts;

  IntegerVector concept_to_add(A.i.used);
  for (size_t i = 0; i < A.i.used; ++i) {
    concept_to_add[i] = A.i.array[i];
  }
  concepts.push_back(concept_to_add);

  BinarySparseVector candB;
  initVector(&candB, n_attributes);

  BinarySparseVector B;
  initVector(&B, n_attributes);

  while (A.i.used < n_attributes) {
    for (int i = n_attributes - 1; i >= 0; --i) {

      bool inA = false;
      for (size_t j = 0; j < A.i.used; ++j) {
        if (A.i.array[j] == i) {
          inA = true;
          break;
        }
      }
      if (inA)
        continue;

      cloneVector(&B, &A);
      insertArray(&(B.i), i);

      binary_compute_closure(&candB, &B, I);

      if (binary_is_set_preceding(A, candB, i)) {
        cloneVector(&A, &candB);
        IntegerVector concept_to_add(A.i.used);
        for (size_t k = 0; k < A.i.used; ++k) {
          concept_to_add[k] = A.i.array[k];
        }
        concepts.push_back(concept_to_add);
        break;
      }
    }
  }

  freeVector(&A);
  freeVector(&candB);
  freeVector(&empty);
  freeVector(&B);

  return concepts;
}
