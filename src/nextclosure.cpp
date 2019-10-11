#include <Rcpp.h>
#include "implication_tree.h"
using namespace Rcpp;

static void chkIntFn3(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt3() {
  return (R_ToplevelExec(chkIntFn3, NULL) == FALSE);
}


// Ganter's Next Closure Algorithm

double cardinal_struct(SparseVector A) {

  double res = 0;
  for (int i = 0; i < A.i.used; i++) {

    res = res + A.x.array[i];

  }

  return res;

}

SparseVector setdifference_struct(SparseVector x,
                                  SparseVector y) {

  SparseVector res;
  initVector(&res, x.length);


  for (int i = 0; i < x.i.used; i++) {

    bool add = true;

    for (int j = 0; j < y.i.used; j++) {

      if (x.i.array[i] == y.i.array[j]) {

        if (y.x.array[j] >= x.x.array[i]) {

          add = false;
          break;

        }

        if (y.i.array[j] > x.i.array[i]) break;

      }

    }

    if (add) {

      insertArray(&(res.i), x.i.array[i]);
      insertArray(&(res.x), x.x.array[i]);

    }

  }


  return res;

}

SparseVector compute_intent_struct (SparseVector V,
                                    NumericMatrix I) {

  SparseVector R;

  initVector(&R, I.ncol());

  int i;

  for (int c = 0; c < I.ncol(); c++) {

    double ms = 1;

    for (int r = 0; r < V.i.used; r++) {

      i = V.i.array[r];

      double tmp = (V.x.array[r] <= I(i, c)) ? 1.0 : I(i, c);

      if (tmp < ms) ms = tmp;

    }

    if (ms > 0) {

      insertArray(&(R.i), c);
      insertArray(&(R.x), ms);

    }

  }

  return(R);

}

SparseVector compute_extent_struct (SparseVector V,
                                    NumericMatrix I) {

  SparseVector R;

  initVector(&R, I.nrow());

  int i;

  for (int r = 0; r < I.nrow(); r++) {

    double ms = 1;

    for (int c = 0; c < V.i.used; c++) {

      i = V.i.array[c];

      double tmp = (V.x.array[c] <= I(r, i)) ? 1 : I(r, i);

      if (tmp < ms) ms = tmp;

    }

    if (ms > 0) {

      insertArray(&(R.i), r);
      insertArray(&(R.x), ms);

    }

  }

  return R;

}

SparseVector compute_closure_struct (SparseVector V,
                                     NumericMatrix I) {

  SparseVector A = compute_extent_struct(V, I);
  SparseVector B = compute_intent_struct(A, I);

  return B;

}

// Functions to compute the next pseudo-closed set

void direct_sum_struct3(SparseVector A,
                        int a_i,
                        double grade_i,
                        int imax,
                        SparseVector *res) {

  reinitVector(res);

  cloneVector(res, A);

  int resp = res->i.used;

  for (int i = 0; i < A.i.used; i++) {

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

void is_subset_tree_struct3(SparseVector A,
                            const struct ImplicationTree t,
                            IntArray *res,
                            bool* black_list) {

  reinitArray(res);

  if (t.COUNT.used > 0) {

    int* counts = (int*)malloc(t.COUNT.used * sizeof(int));

    std::copy(&t.COUNT.array[0], &t.COUNT.array[t.COUNT.used], counts);

    for (int i = 0; i < A.i.used; i++) {

      int y = A.i.array[i];
      double a = A.x.array[i];

      for (int j = 0; j < t.DEGREE[y].used; j++) {

        if (t.DEGREE[y].array[j] <= a) {

          counts[t.LIST[y].array[j]] = counts[t.LIST[y].array[j]] - 1;

          if ((counts[t.LIST[y].array[j]] == 0) & (black_list[t.LIST[y].array[j]])) {

            insertArray(res, t.LIST[y].array[j]);

          }

        }

      }

    }

    free(counts);
  }

}

void set_union_sparsevector(SparseVector RHS,
                            IntArray subsets,
                            SparseVector *res2) {

  int n = subsets.used;

  int num_rows = res2->length;
  reinitVector(res2);

  double *v = (double*)malloc(num_rows * sizeof(double));

  for (int i = 0; i < num_rows; i++) {

    v[i] = 0.0;

  }

  for (int x_index = 0; x_index < n; x_index++) {

    int start_index = RHS.p.array[subsets.array[x_index]];
    int end_index = RHS.p.array[subsets.array[x_index] + 1];

    for (int j = start_index; j < end_index; j++) {

      if (RHS.x.array[j] > v[RHS.i.array[j]]) {

        v[RHS.i.array[j]] = RHS.x.array[j];

      }

    }

  }

  for (int i = 0; i < num_rows; i++) {

    if (v[i] > 0) {

      insertArray(&(res2->i), i);
      insertArray(&(res2->x), v[i]);

    }

  }

  free(v);

}

void setunion_struct2(SparseVector x,
                      SparseVector y,
                      SparseVector *res) {


  int j = 0;

  for (int i = 0; i < x.i.used; i++) {

    while ((j < y.i.used) & (y.i.array[j] < x.i.array[i])) {

      insertArray(&(res->i), y.i.array[j]);
      insertArray(&(res->x), y.x.array[j]);
      j++;

    }

    if (y.i.array[j] == x.i.array[i]) {

      if (x.x.array[i] > y.x.array[j]) {

        insertArray(&(res->i), x.i.array[i]);
        insertArray(&(res->x), x.x.array[i]);
        j++;

      } else {

        insertArray(&(res->i), y.i.array[j]);
        insertArray(&(res->x), y.x.array[j]);
        j++;

      }

    } else {

      insertArray(&(res->i), x.i.array[i]);
      insertArray(&(res->x), x.x.array[i]);

    }

  }

  while (j < y.i.used) {

    insertArray(&(res->i), y.i.array[j]);
    insertArray(&(res->x), y.x.array[j]);
    j++;

  }

}

void semantic_closure_tree4(SparseVector A,
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

    is_subset_tree_struct3(A, t, &subsets, black_list);

    while (subsets.used > 0) {

      set_union_sparsevector(RHS, subsets, &res2);

      setunion_struct2(*res, res2, &res3);

      cloneVector(res, res3);

      reinitVector(&res2);
      reinitVector(&res3);

      for (int i = 0; i < subsets.used; i++) {

        black_list[subsets.array[i]] = false;

      }

      is_subset_tree_struct3(*res, t, &subsets, black_list);

    }

    freeVector(&res2);
    freeVector(&res3);

    freeArray(&subsets);

    free(black_list);

  }

}

bool is_set_preceding_struct(SparseVector B,
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
  for (int i = 0; i < B.i.used; i++) {

    if (B.i.array[i] < a_i) {

      insertArray(&bi_lt_a_i, B.i.array[i]);
      insertArray(&bx_lt_a_i, B.x.array[i]);

    }

    if (B.i.array[i] == a_i) {

      bx_at_a_i = B.x.array[i];

    }

  }

  for (int i = 0; i < C.i.used; i++) {

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

  for (int i = 0; i < ci_lt_a_i.used; i++) {

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

SparseVector next_closure_implications_tree3(SparseVector A, int i,
                                             int imax,
                                             ListOf<NumericVector> grades_set,
                                             ImplicationTree t,
                                             SparseVector LHS,
                                             SparseVector RHS,
                                             StringVector attrs) {


  SparseVector candB;
  initVector(&candB, A.length);

  int n_grades = grades_set.size();
  SparseVector candB2;
  initVector(&candB2, A.length);

  for (int a_i = i - 1; i >= 0; a_i--) {

    n_grades = grades_set[a_i].size();

    for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

      direct_sum_struct3(A, a_i, grades_set[a_i][grade_idx], imax, &candB);

      semantic_closure_tree4(candB, t, LHS, RHS, &candB2);
      cloneVector(&candB, candB2);

      if (is_set_preceding_struct(A, candB, a_i, grades_set[a_i][grade_idx])) {

        freeVector(&candB2);
        return candB;

      }

    }

  }

  Rprintf("Something went wrong...\n");

  return candB;

}


// [[Rcpp::export]]
List ganters_algorithm_implications_tree_final(NumericMatrix I,
                                               List grades_set,
                                               StringVector attrs,
                                               bool verbose = false) {

  int n_attributes = attrs.size();

  SparseVector concepts;
  SparseVector LHS, RHS;
  initVector(&concepts, n_attributes);
  initVector(&LHS, n_attributes);
  initVector(&RHS, n_attributes);

  SparseVector empty, B, rhs;

  initVector(&empty, n_attributes);
  initVector(&B, n_attributes);
  initVector(&rhs, n_attributes);

  ImplicationTree tree;
  initImplicationTree(&tree, n_attributes);

  SparseVector A = compute_closure_struct(empty, I);

  if (cardinal_struct(A) > 0) {

    add_column(&LHS, empty);
    add_column(&RHS, A);

  } else {

    add_column(&concepts, A);

  }

  int count = 0;

  double pctg, old_pctg = 0;

  while ((cardinal_struct(A) < n_attributes)){

    A = next_closure_implications_tree3(A,
                                        n_attributes,
                                        n_attributes,
                                        grades_set,
                                        tree, LHS, RHS,
                                        attrs);

    if (verbose) {

      pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;

      if (pctg != old_pctg) {

        Rprintf("Completed = %.2f\n %", pctg);
        old_pctg = pctg;

      }

    }

    B = compute_closure_struct(A, I);

    rhs = setdifference_struct(B, A);

    if (cardinal_struct(rhs) == 0) {

      // Concept
      add_column(&concepts, A);

      if (verbose) {

        Rprintf("Added concept:\n");
        printVector(A, attrs);
        Rprintf("\n");

      }

    } else {

      add_column(&LHS, A);
      add_column(&RHS, rhs);

      if (verbose) {

        Rcout << "Added implication to basis" << std::endl << std::endl << std::endl;
        printImpl(A, rhs, attrs);

      }

      addImplicationToTree(&tree, A);

      count++;

      if (verbose) {

        if (count % 10 == 0) Rprintf("%u\n", count);

      }

    }

    if (checkInterrupt3()) { // user interrupted ...

      List res = List::create(_["concepts"] = SparseToS4(concepts),
                              _["LHS"] = SparseToS4(LHS),
                              _["RHS"] = SparseToS4(RHS));

      Rprintf("User interrupted.\n");
      return res;

    }

  }

  List res = List::create(_["concepts"] = SparseToS4(concepts),
                          _["LHS"] = SparseToS4(LHS),
                          _["RHS"] = SparseToS4(RHS));

  if (verbose)
    Rprintf("Finished.\n");

  return res;

}
