#include <Rcpp.h>
#include "set_operations_galois.h"
using namespace Rcpp;

static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}


// Ganter's Next Closure Algorithm


// Functions to compute the next pseudo-closed set

void compute_direct_sum(SparseVector A,
                        int a_i,
                        double grade_i,
                        int imax,
                        SparseVector *res) {

  reinitVector(res);

  cloneVector(res, A);

  int resp = res->i.used;

  for (size_t i = 0; i < A.i.used; i++) {

    if (A.i.array[i] >= a_i) {

      resp = i;
      break;

    }

  }

  assignUsed(&(res->i), resp);
  assignUsed(&(res->x), resp);

  insertArray(&(res->i), a_i);
  insertArray(&(res->x), grade_i);

}

void semantic_closure(SparseVector A,
                      ImplicationTree t,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res) {


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
    bool* black_list = (bool*)malloc(n * sizeof(bool));

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

bool is_set_preceding(SparseVector B,
                      SparseVector C,
                      int a_i,
                      double grade_i) {

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

  if (cx_at_a_i != grade_i) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    return false;

  }

  if (bx_at_a_i >= cx_at_a_i) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    return false;

  }

  if (ci_lt_a_i.used != bi_lt_a_i.used) {

    freeArray(&cx_lt_a_i);
    freeArray(&bx_lt_a_i);
    freeArray(&ci_lt_a_i);
    freeArray(&bi_lt_a_i);

    return false;

  }

  for (size_t i = 0; i < ci_lt_a_i.used; i++) {

    if (ci_lt_a_i.array[i] != bi_lt_a_i.array[i]) {

      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);

      return false;

    }
    if (cx_lt_a_i.array[i] != bx_lt_a_i.array[i]) {

      freeArray(&cx_lt_a_i);
      freeArray(&bx_lt_a_i);
      freeArray(&ci_lt_a_i);
      freeArray(&bi_lt_a_i);

      return false;

    }

  }

  freeArray(&cx_lt_a_i);
  freeArray(&bx_lt_a_i);
  freeArray(&ci_lt_a_i);
  freeArray(&bi_lt_a_i);

  return true;

}

void compute_next_closure(SparseVector A, int i,
                                  int imax,
                                  ListOf<NumericVector> grades_set,
                                  ImplicationTree t,
                                  SparseVector LHS,
                                  SparseVector RHS,
                                  StringVector attrs,
                                  SparseVector *candB) {


  // SparseVector candB;
  // initVector(&candB, A.length);

  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);

  bool exit = false;

  for (int a_i = i - 1; i >= 0; a_i--) {

    n_grades = grades_set[a_i].size();

    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {

      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, candB);

      semantic_closure(*candB, t, LHS, RHS, &candB2);

      if (is_set_preceding(A, candB2, a_i, grades_set[a_i][grade_idx])) {

        cloneVector(candB, candB2);
        freeVector(&candB2);

        exit = true;

        // return candB;

      }

      if (exit) break;

    }

    if (exit) break;

  }

  // Rprintf("Something went wrong...\n");
  //
  // return candB;

}


// [[Rcpp::export]]
List next_closure_implications(NumericMatrix I,
                               List grades_set,
                               StringVector attrs,
                               bool save_concepts = true,
                               bool verbose = false) {

  int n_attributes = attrs.size();

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
  initImplicationTree(&tree, n_attributes);

  SparseVector A = compute_closure(empty, I);

  if (cardinal(A) > 0) {

    add_column(&LHS, empty);
    add_column(&RHS, A);
    addImplicationToTree(&tree, empty);


    if (verbose) {

      Rcout << "Added initial implication to basis" << std::endl << std::endl << std::endl;
      printVector(A, attrs);
      Rcout << std::endl << std::endl;
      // printImpl(empty, A, attrs);

      n_imp++;

    }

  }

  if (save_concepts) {

    add_column(&concepts, A);
    add_column(&extents, compute_extent(A, I));

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

  while ((cardinal(A) < n_attributes)){

    compute_next_closure(A,
                             n_attributes,
                             n_attributes,
                             grades_set,
                             tree, LHS, RHS,
                             attrs, &B);

    // cloneVector(&A, B);
    A = B;

    if (verbose) {

      pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;

      if (pctg != old_pctg) {

        Rprintf("Completed = %.2f\n %", pctg);
        old_pctg = pctg;

      }

    }

    B = compute_closure(A, I);

    rhs = setdifference(B, A);

    if (cardinal(rhs) == 0) {

      // Concept
      if (save_concepts) {

        add_column(&concepts, A);
        add_column(&extents, compute_extent(A, I));

        if (verbose) {

          Rprintf("Added concept:\n");
          printVector(A, attrs);
          Rprintf("\n");

        }

      }

    } else {

      add_column(&LHS, A);
      add_column(&RHS, rhs);

      if (verbose) {

        Rcout << "Added implication " << n_imp++ << " to basis" << std::endl;
        printImpl(A, rhs, attrs);

      }

      addImplicationToTree(&tree, A);

      count++;

      if (verbose) {

        if (count % 10 == 0) Rprintf("%u\n", count);

      }

    }

    if (checkInterrupt()) { // user interrupted ...

      List res = List::create(_["concepts"] = SparseToS4_fast(concepts),
                              _["extents"] = SparseToS4_fast(extents),
                              _["LHS"] = SparseToS4_fast(LHS),
                              _["RHS"] = SparseToS4_fast(RHS));

      Rprintf("User interrupted.\n");
      return res;

    }

  }

  List res = List::create(_["concepts"] = SparseToS4_fast  (concepts),
                          _["extents"] = SparseToS4_fast(extents),
                          _["LHS"] = SparseToS4_fast(LHS),
                          _["RHS"] = SparseToS4_fast(RHS));

  if (verbose)
    Rprintf("Finished.\n");

  return res;

}

SparseVector compute_next_intent(SparseVector A,
                                 NumericMatrix I,
                                 int i,
                                 int imax,
                                 ListOf<NumericVector> grades_set) {


  SparseVector candB;
  initVector(&candB, A.length);

  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);

  for (int a_i = i - 1; i >= 0; a_i--) {

    n_grades = grades_set[a_i].size();

    for (int grade_idx = 1; grade_idx < n_grades; grade_idx++) {

      compute_direct_sum(A, a_i, grades_set[a_i][grade_idx], imax, &candB);

      candB2 = compute_closure(candB, I);
      cloneVector(&candB, candB2);

      if (is_set_preceding(A, candB, a_i, grades_set[a_i][grade_idx])) {

        freeVector(&candB2);
        return candB;

      }

    }

  }

  Rprintf("Something went wrong...\n");

  return candB;

}

// [[Rcpp::export]]
List next_closure_concepts(NumericMatrix I,
                           List grades_set,
                           StringVector attrs,
                           bool verbose = false) {

  int n_attributes = attrs.size();

  SparseVector concepts;
  SparseVector extents;
  initVector(&concepts, n_attributes);
  initVector(&extents, I.nrow());

  SparseVector empty, B;

  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);


  SparseVector A = compute_closure(empty, I);

  add_column(&concepts, A);
  add_column(&extents, compute_extent(A, I));

  if (verbose) {

    Rprintf("Added concept:\n");

    if (cardinal(A) > 0) {

      printVector(A, attrs);

    } else {

      Rprintf("{}");

    }

    Rprintf("\n");

  }

  double pctg, old_pctg = 0;

  while ((cardinal(A) < n_attributes)){

    // A = compute_next_closure(A,
    //                          n_attributes,
    //                          n_attributes,
    //                          grades_set,
    //                          tree, LHS, RHS,
    //                          attrs);

    A = compute_next_intent(A, I,
                            n_attributes,
                            n_attributes,
                            grades_set);

    if (verbose) {

      pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;

      if (pctg != old_pctg) {

        Rprintf("Completed = %.2f\n %", pctg);
        old_pctg = pctg;

      }

    }

    // B = compute_closure(A, I);
    //
    // rhs = setdifference(B, A);
    //
    // if (cardinal(rhs) == 0) {

    // Concept
    add_column(&concepts, A);
    add_column(&extents, compute_extent(A, I));

    if (verbose) {

      Rprintf("Added concept:\n");
      printVector(A, attrs);
      Rprintf("\n");

    }



    // }

    if (checkInterrupt()) { // user interrupted ...

      List res = List::create(_["concepts"] = SparseToS4_fast(concepts),
                              _["extents"] = SparseToS4_fast(extents));

      Rprintf("User interrupted.\n");
      return res;

    }

  }

  List res = List::create(_["concepts"] = SparseToS4_fast(concepts),
                          _["extents"] = SparseToS4_fast(extents));

  if (verbose)
    Rprintf("Finished.\n");

  return res;

}
