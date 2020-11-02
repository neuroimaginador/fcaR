#include <Rcpp.h>
#include "aux_functions.h"

using namespace Rcpp;

bool is_set_preceding_fcbo(SparseVector B,
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

void setdifference_fcbo(SparseVector x,
                   SparseVector y,
                   SparseVector *res) {

  reinitVector(res);

  for (size_t i = 0; i < x.i.used; i++) {

    bool add = true;

    for (size_t j = 0; j < y.i.used; j++) {

      if (x.i.array[i] == y.i.array[j]) {

        if (y.x.array[j] >= x.x.array[i]) {

          add = false;
          break;

        }

        if (y.i.array[j] > x.i.array[i]) break;

      }

    }

    if (add) {

      insertArray(&(res->i), x.i.array[i]);
      insertArray(&(res->x), x.x.array[i]);

    }

  }

  insertArray(&(res->p), 0);
  insertArray(&(res->p), res->i.used);

}

void semantic_closure_fcbo(SparseVector A,
                      ImplicationTree t,
                      SparseVector LHS,
                      SparseVector RHS,
                      SparseVector *res) {


  int n_attributes = A.length;

  reinitVector(res);

  cloneVector(res, A);

  // Rcout << "RHS " << RHS.p.used << std::endl;

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

        // Rcout << "We have used implication number: " << subsets.array[i] << std::endl;

      }

      is_subset(*res, t, &subsets, black_list);

    }

    freeVector(&res2);
    freeVector(&res3);

    freeArray(&subsets);

    free(black_list);

  }

}

void FuzzyFastGenerateFrom4_imps(double* I,
                                   const SparseVector myI,
                                   int n_objects,
                                   int n_attributes,
                                   int n_grades,
                                   StringVector attrs,
                                   double* grades_set,
                                   SparseVector *extents,
                                   SparseVector *intents,
                                   SparseVector *LHS,
                                   SparseVector *RHS,
                                   ImplicationTree *tree,
                                   bool add,
                                   SparseVector qA, // extent
                                   SparseVector qB, // intent
                                   SparseVector qPremise, // premise
                                   int id_col,
                                   int y,
                                   int gr,
                                   double* Ny,
                                   int* closure_count,
                                   int* intent_count,
                                   int depth = 0) {

  Rcout << "** DEPTH = " << depth << " " << std::endl;
  Rcout << "** y = " << y << " " << std::endl;

  SparseVector B, premise, closure, diff;
  initVector(&B, qB.length);
  initVector(&premise, qPremise.length);
  initVector(&closure, n_attributes);
  initVector(&diff, n_attributes);
  get_column(&B, qB, id_col);
  get_column(&premise, qPremise, id_col);


  // printArray(qPremise.i);
  //
  semantic_closure_fcbo(premise,
                        *tree,
                        *LHS,
                        *RHS,
                        &closure);

  // Check that closure is after premise
  setdifference_fcbo(closure, premise, &diff);

  if ((diff.i.used == 0) ||
      (premise.i.used == 0) ||
      (diff.i.array[0] > premise.i.array[premise.i.used - 1])) {

  // if ((closure.i.used == 0) ||
  // (premise.i.used == 0) ||
  // // (is_set_preceding_fcbo(premise, closure, y, grades_set[gr]))) {
  // (closure.i.array[0] == premise.i.array[0])) {

    reinitVector(&diff);
    setdifference_fcbo(B, closure, &diff);
    // printArray(diff.i);

    // equal = cardinal(diff) == 0;

    if (cardinal(diff) > 0) {

      // reinitVector(&diff);
      // setdifference_fcbo(B, premise, &diff);

      // Add new implication:
      add_column(LHS, closure);
      add_column(RHS, diff);
      addImplicationToTree(tree, closure);

      Rcout << "Added implication to basis" << std::endl;
      printImpl(closure, diff, attrs);

      // Rcout << "Tree: " << std::endl;
      // printArray(tree->COUNT);
      // printArray(tree->CARD);


    } else {

      Rcout << "Already contained implication:" << std::endl;

    }

    Rcout << "Premise: " << std::endl;
    printArray(premise.i);
    printArray(premise.x);
    Rcout << "Semantic closure: " << std::endl;
    printArray(closure.i);
    printArray(closure.x);
    Rcout << "Context closure: " << std::endl;
    printArray(B.i);
    printArray(B.x);
    Rcout << "difference: " << std::endl;
    printArray(diff.i);
    printArray(diff.x);

    // reinitVector(&closure);
    // cloneVector(&closure, B);

    // if (LHS->p.used < 50) {
    //
    //   Rcout << "LHS: " << std::endl;
    //   printArray(LHS->i);
    //   printArray(LHS->x);
    //   Rcout << "RHS: " << std::endl;
    //   printArray(RHS->i);
    //   printArray(RHS->x);
    //
    // }

    Rcout << std::endl;

  } else {

    Rcout << "Rejected implication" << std::endl;
    // setdifference_fcbo(B, closure, &diff);

    // printImpl(closure, diff, attrs);

    Rcout << "Premise: " << std::endl;
    printArray(premise.i);
    printArray(premise.x);
    Rcout << "Semantic closure: " << std::endl;
    printArray(closure.i);
    printArray(closure.x);
    // Rcout << "Context closure: " << std::endl;
    // printArray(B.i);
    // printArray(B.x);

  }

  // bool equal = true;
  // if (B.i.used != closure.i.used) equal = false;
  // if (equal) {
  //
  //   for (int i = 0; i < B.i.used; i++) {
  //
  //     if (B.i.array[i] != closure.i.array[i]) {
  //
  //       equal = false;
  //       break;
  //
  //     }
  //
  //   }
  //
  // }
  //
  // if (equal) {
  //
  //   for (int i = 0; i < B.i.used; i++) {
  //
  //     if (B.x.array[i] != closure.x.array[i]) {
  //
  //       equal = false;
  //       break;
  //
  //     }
  //
  //   }
  //
  // }



  // freeVector(&B);
  // freeVector(&premise);
  // reinitVector(&premise);
  // cloneVector(&premise, closure);
  freeVector(&closure);
  freeVector(&diff);

  if (!add) {

    freeVector(&B);
    freeVector(&premise);

    return;

  }

  SparseVector A;
  initVector(&A, qA.length);
  // initVector(&B, qB.length);
  // initVector(&premise, qPremise.length);
  get_column(&A, qA, id_col);
  // get_column(&B, qB, id_col);
  // get_column(&premise, qPremise, id_col);

  // Add <A, B> to list of concepts.
  add_column(extents, A);
  add_column(intents, B);

  if ((cardinal(B) == n_attributes) || (y >= n_attributes)) return;

  double* Bv = (double*)calloc(B.length, sizeof(double));
  for (int i = 0; i < B.i.used; i++) {

    Bv[B.i.array[i]] = B.x.array[i];

  }

  // Prepare to queue next recursion step.
  SparseVector queue_A, queue_B, queue_premise;
  initVector(&queue_A, qA.length);
  initVector(&queue_B, qB.length);
  initVector(&queue_premise, qPremise.length);

  IntArray queue_y, queue_gr, queue_add;
  initArray(&queue_y, n_attributes * n_grades);
  initArray(&queue_gr, n_attributes * n_grades);
  initArray(&queue_add, n_attributes * n_grades);

  double* Mj = (double*)malloc(n_attributes * n_attributes * n_grades * sizeof(double));

  memcpy(Mj, Ny, n_attributes * n_attributes * n_grades * sizeof(double));

  if ((cardinal(B) < n_attributes) && (y < n_attributes)) {

    double B_j;

    SparseVector foo, D, pr;
    initVector(&foo, n_objects);
    initVector(&pr, n_attributes);
    initVector(&D, n_attributes);

    initVector(&closure, n_attributes);

    for (int j = n_attributes - 1; j >= y; j--) {

      Rcout << "Probando j = " << j << " " << std::endl;

      B_j = Bv[j];

      if (B_j == 1)
        continue;

      for (int g_idx = 0; g_idx < n_grades; g_idx++) {

        Rcout << "Probando g_idx = " << g_idx << " " << std::endl;

        // conditions to avoid this step:
        if (B_j > grades_set[g_idx]) {

          Rcout << "Avoid #1" << std::endl;

          continue;

        }

        if ((j == y) && (g_idx >= gr)) {

          Rcout << "Avoid #2" << std::endl;

          continue;

        }

        reinitVector(&pr);
        bool added = false;
        for (int i = 0; i < premise.i.used; i++) {

          int idx = premise.i.array[i];
          if (idx == j) {

            // Rcout << "Añadido" << std::endl;

            insertArray(&(pr.i), j);
            insertArray(&(pr.x), grades_set[g_idx]);

            added = true;
            continue;

          }

          if (idx < j) {

            insertArray(&(pr.i), idx);
            insertArray(&(pr.x), premise.x.array[i]);

          } else {

            if (!added) {

              // Rcout << "Añadido" << std::endl;

              insertArray(&(pr.i), j);
              insertArray(&(pr.x), grades_set[g_idx]);

              added = true;

            }

            insertArray(&(pr.i), idx);
            insertArray(&(pr.x), premise.x.array[i]);

          }

        }

        if (!added) {

          // Rcout << "Añadido" << std::endl;

          insertArray(&(pr.i), j);
          insertArray(&(pr.x), grades_set[g_idx]);

          added = true;

        }

        insertArray(&(pr.p), 0);
        insertArray(&(pr.p), pr.i.used);

        Rcout << "New premise:" << std::endl;
        printArray(pr.i);
        printArray(pr.x);

        Rcout << "Parent intent:" << std::endl;
        printArray(B.i);
        printArray(B.x);

        // reinitVector(&closure);
        //
        // semantic_closure_fcbo(pr,
        //                       *tree,
        //                       *LHS,
        //                       *RHS,
        //                       &closure);
        //
        // Rcout << "Semantic closure: " << std::endl;
        // printArray(closure.i);
        // printArray(closure.x);


        // if (j > 0) {
        //
        //   int s = 0;
        //   for (s = 0; s < j; s++) {
        //
        //     if (Ny[g_idx * n_attributes * n_attributes + j * n_attributes + s] > Bv[s])
        //       break;
        //
        //   }
        //
        //   if (s < j) {
        //
        //     Rcout << "Avoid #3" << std::endl;
        //
        //     continue;
        //
        //   }
        //
        // }

        // Obtain next concept
        reinitVector(&foo);
        get_column(&foo, myI, g_idx * n_attributes + j);

        intersect(&foo, A);

        reinitVector(&D);
        compute_intent(&D, foo, I, n_objects, n_attributes);
        (*intent_count) += n_attributes;

        // Rcout << "Computed" << std::endl;

        Rcout << "Context closure: " << std::endl;
        printArray(D.i);
        printArray(D.x);

        (*closure_count)++;

        bool canonical = true;

        if (j > 0) {

          int s = 0;

          for (int is = 0; is < D.i.used; is++) {

            int iD = D.i.array[is];

            if (iD >= j)
              break;

            while (s < iD) {

              if (Bv[s] > 0) {

                canonical = false;
                break;

              }
              s++;

            }

            if (!canonical) break;

            if (s == iD) {

              if (Bv[s] < D.x.array[is]) {

                canonical = false;
                break;

              }
              s++;

            }

          }

        }

        if (canonical) {

          double D_j = get_element(D, j);

          // Probably we could use this knowledge to
          // accelerate the algorithm.
          if (D_j > grades_set[g_idx]) {

            add_column(&queue_A, foo);
            add_column(&queue_B, D);
            insertArray(&queue_gr, g_idx);
            insertArray(&queue_y, j);
            insertArray(&queue_add, 0);
            add_column(&queue_premise, pr);

            continue;

          }

          if (D_j == B_j)
            continue;

          Rcout << "Aceptado" << std::endl;

          add_column(&queue_A, foo);
          add_column(&queue_B, D);
          insertArray(&queue_gr, g_idx);
          insertArray(&queue_y, j);
          insertArray(&queue_add, 1);
          add_column(&queue_premise, pr);

        } else {

          for (int idx = 0; idx < D.i.used; idx++) {

            int linear_index = g_idx * n_attributes * n_attributes + j * n_attributes + D.i.array[idx];
            Mj[linear_index] = D.x.array[idx];

          }

          Rcout << "Rechazado" << std::endl;


          add_column(&queue_A, foo);
          add_column(&queue_B, D);
          insertArray(&queue_gr, g_idx);
          insertArray(&queue_y, j);
          insertArray(&queue_add, 0);
          add_column(&queue_premise, pr);

        }

      }

    }

    freeVector(&foo);
    freeVector(&D);
    freeVector(&pr);

  }

  bool to_add;
  for (int cols = 0; cols < queue_y.used; cols++) {

    to_add =  (queue_add.array[cols] == 1) ? true : false;

    FuzzyFastGenerateFrom4_imps(I,
                                  myI,
                                  n_objects,
                                  n_attributes,
                                  n_grades,
                                  attrs,
                                  grades_set,
                                  extents,
                                  intents,
                                  LHS,
                                  RHS,
                                  tree,
                                  to_add,
                                  queue_A,
                                  queue_B,
                                  queue_premise,
                                  cols,
                                  queue_y.array[cols],
                                  queue_gr.array[cols],
                                  Mj,
                                  closure_count,
                                  intent_count,
                                  depth + 1);

  }

  freeVector(&queue_A);
  freeVector(&queue_B);
  freeArray(&queue_y);
  freeArray(&queue_gr);
  freeArray(&queue_add);
  freeVector(&queue_premise);

  free(Mj);
  Mj = NULL;

  free(Bv);
  Bv = NULL;

  // free(D1);
  // D1 = NULL;

  freeVector(&A);
  freeVector(&B);
  freeVector(&premise);

}

// [[Rcpp::export]]
List FuzzyFastCbo_C4_imps(NumericMatrix I,
                            StringVector attrs,
                            NumericVector grades_set) {

  int n_objects = I.nrow();
  int n_attributes = I.ncol();
  int n_grades = grades_set.size();

  int closure_count = 0;
  int intent_count = 0;
  // double memory = 0;

  SparseVector myI;
  initMatrix(&myI, n_objects);
  SparseVector I1;
  initVector(&I1, n_objects);

  for (int g = 0; g < n_grades; g++) {

    for (int i = 0; i < n_attributes; i++) {

      reinitVector(&I1);

      as_sparse(&I1, I.begin(), n_objects, n_attributes, i);

      zadeh_I(grades_set[g], &I1);
      add_column(&myI, I1);
      // freeVector(&I1);

    }

  }

  freeVector(&I1);

  SparseVector B; // Empty intent
  initVector(&B, n_attributes);

  SparseVector A;
  initVector(&A, n_objects);
  compute_extent(&A, B, I.begin(), n_objects, n_attributes);
  freeVector(&B);

  SparseVector C;
  initVector(&C, n_attributes);
  compute_intent(&C, A, I.begin(), n_objects, n_attributes);
  intent_count += n_attributes;

  SparseVector premise;
  initVector(&premise, n_attributes);


  closure_count++;

  SparseVector intents, LHS, RHS;
  SparseVector extents;
  initVector(&intents, n_attributes);
  initVector(&LHS, n_attributes);
  initVector(&RHS, n_attributes);
  initVector(&extents, n_objects);

  double* Ny = (double*)calloc(n_attributes * n_attributes * n_grades,
                sizeof(double));

  ImplicationTree tree;
  initImplicationTree(&tree, n_attributes);

  FuzzyFastGenerateFrom4_imps(I.begin(),
                                myI,
                                n_objects,
                                n_attributes,
                                n_grades,
                                attrs,
                                grades_set.begin(),
                                &extents,
                                &intents,
                                &LHS,
                                &RHS,
                                &tree,
                                true,
                                A,
                                C,
                                premise,
                                0,
                                0,
                                n_grades + 1,
                                Ny,
                                &closure_count,
                                &intent_count,
                                0);

  free(Ny);

  S4 intents_S4 = SparseToS4_fast(intents);
  S4 extents_S4 = SparseToS4_fast(extents);

  S4 lhs_S4 = SparseToS4_fast(LHS);
  S4 rhs_S4 = SparseToS4_fast(RHS);

  freeVector(&A);
  freeVector(&C);
  freeVector(&intents);
  freeVector(&LHS);
  freeVector(&RHS);
  freeVector(&extents);
  freeVector(&premise);

  List res = List::create(
    _["intents"] = intents_S4,
    _["extents"] = extents_S4,
    _["lhs"] = lhs_S4,
    _["rhs"] = rhs_S4,
    _["closure_count"] = closure_count,
    _["intent_count"] = (double)intent_count / (double)(n_attributes * n_grades));

  return res;

}


