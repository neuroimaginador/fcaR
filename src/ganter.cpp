#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>

using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

double _I_(double x, double y) {

  if (x <= y) return(1);

  return(y);

}

double setdiffC(double x, double y) {

  if (y >= x) return(0);

  return(x);

}

NumericVector setunionC(NumericVector x, NumericVector y) {

  int n = x.size();
  NumericVector res = clone(x);

  for (int i = 0; i < n; i++) {

    if (y[i] > x[i]) res[i] = y[i];

  }

  return res;

}

// [[Rcpp::export]]
NumericVector intent_C(NumericVector A, NumericMatrix I) {

  NumericVector res(I.ncol());

  for (int c = 0; c < I.ncol(); c++) {

    double ms = 1;

    for (int r = 0; r < I.nrow(); r++) {

      double tmp = _I_(A[r], I(r, c));

      if (tmp < ms) ms = tmp;

    }

    res[c] = ms;

  }

  return(res);

}
// [[Rcpp::export]]
NumericVector extent_C(NumericVector A, NumericMatrix I) {

  NumericVector res(I.nrow());

  // Rprintf("nrow = %u, ncol = %u\n", I.nrow(), I.ncol());

  for (int r = 0; r < I.nrow(); r++) {

    double ms = 1;

    for (int c = 0; c < I.ncol(); c++) {

      double tmp = _I_(A[c], I(r, c));

      if (tmp < ms) ms = tmp;

    }

    res[r] = ms;

  }

  return(res);
}

// [[Rcpp::export]]
NumericVector closure_C(NumericVector A, NumericMatrix I) {

  return(intent_C(extent_C(A, I), I));

}

// [[Rcpp::export]]
NumericVector direct_sum_C(NumericVector A, int a_i,
                           double grade_i, int imax) {

  NumericVector the_sum = clone(A);

  for (int i = a_i + 1; i < A.size(); i++) {

    the_sum[i] = 0;

  }

  the_sum[a_i] = grade_i;

  return the_sum;

}

// [[Rcpp::export]]
bool is_set_preceding_C(NumericVector B, NumericVector C,
                        int a_i, double grade_i) {

  if (B[a_i] >= C[a_i]) return false;

  if (C[a_i] != grade_i) return false;

  for (int i = 0; i < a_i; i++) {

    if (B[i] != C[i]) return false;

  }

  return true;

}

// [[Rcpp::export]]
NumericVector next_closure_C(NumericVector A, int i, int imax,
                             NumericVector grades_set,
                             NumericMatrix I) {

  int n_grades = grades_set.size();
  NumericVector candB(A.size());

  for (int a_i = i - 1; i >= 0; a_i--) {

    for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

      candB = direct_sum_C(A, a_i, grades_set[grade_idx], imax);

      candB = closure_C(candB, I);

      if (is_set_preceding_C(A, candB, a_i, grades_set[grade_idx])) {

        return candB;

      }

    }

  }

  return candB * 0;

}

// [[Rcpp::export]]
List ganters_algorithm(NumericMatrix I,
                       NumericVector grades_set,
                       int n_attributes) {

  std::list <NumericVector> res2;
  NumericVector empty(n_attributes);

  NumericVector A = closure_C(empty, I);

  List res;

  int count = 0;

  while (sum(A) < n_attributes) {

    A = next_closure_C(A, n_attributes, n_attributes, grades_set, I);

    res2.push_back(A);//[count++] = A;

    count++;

  }

  return wrap(res2);

}

// [[Rcpp::export]]
LogicalVector is_subset_one(NumericVector A,
                            std::list<NumericVector> LHS) {

  int n = LHS.size();

  LogicalVector res(n);
  int n_attributes = A.size();

  std::list<NumericVector>::iterator it;
  int count = 0;

  for (it = LHS.begin(); it != LHS.end(); ++it ) {

    res[count] = true;

    for (int i = 0; i < n_attributes; i++) {

      if ((*it)[i] > A[i]) {

        res[count] = false;
        break;

      }

    }

    count++;

  }

  return res;

}

// [[Rcpp::export]]
NumericVector semantic_closure_C(NumericVector A,
                                 std::list<NumericVector> LHS,
                                 std::list<NumericVector> RHS) {

  int n = LHS.size();

  int n_attributes = A.size();
  NumericVector res = clone(A);

  LogicalVector subsets(n);
  LogicalVector black_list(n);
  int idx;

  for (int i = 0; i < n; i++) {

    black_list[i] = true;

  }

  subsets = is_subset_one(A, LHS);

  while (sum(subsets) > 0) {

    std::list<NumericVector>::iterator it;
    idx = 0;

    for (it = RHS.begin(); it != RHS.end(); ++it ) {

      if (subsets[idx]) {

        res = setunionC(res, (*it));
        black_list[idx] = false;

      }

      idx++;

    }

    subsets = is_subset_one(res, LHS) & black_list;

  }

  return res;

}

// [[Rcpp::export]]
NumericVector next_closure_implications_C(NumericVector A, int i,
                                          int imax,
                                          List grades_set,
                                          std::list<NumericVector> LHS,
                                          std::list<NumericVector> RHS) {

  int n_grades = grades_set.size();
  NumericVector candB(A.size());
  NumericVector gr;

  for (int a_i = i - 1; i >= 0; a_i--) {

    gr = grades_set[a_i];
    n_grades = gr.size();

    for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

      candB = direct_sum_C(A, a_i, gr[grade_idx], imax);

      candB = semantic_closure_C(candB, LHS, RHS);

      if (is_set_preceding_C(A, candB, a_i, gr[grade_idx])) {

        return candB;

      }

    }

  }

  return candB * 0;

}

// [[Rcpp::export]]
NumericVector setdifferenceC(NumericVector x, NumericVector y) {

  int n = x.size();
  NumericVector res(n);

  for (int i = 0; i < n; i++) {

    res[i] = setdiffC(x[i], y[i]);

  }

  return res;

}

static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}


// [[Rcpp::export]]
List ganters_algorithm_implications(NumericMatrix I,
                                    List grades_set,
                                    int n_attributes) {

  Timer timer;

  timer.step("start");

  std::list <NumericVector> res2;
  std::list <NumericVector> LHS;
  std::list <NumericVector> RHS;

  NumericVector empty(n_attributes);
  NumericVector B(n_attributes);
  NumericVector rhs(n_attributes);

  NumericVector A = closure_C(empty, I);

  if (sum(A) > 0) {

    LHS.push_back(empty);
    RHS.push_back(A);

  } else {

    res2.push_back(A);

  }

  int count = 0;

  while (sum(A) < n_attributes) {

    timer.step("in");
    A = next_closure_implications_C(A, n_attributes, n_attributes,
                                    grades_set, LHS, RHS);

    timer.step("next_closure");
    // Rprintf("Antes de closure\n");
    B = closure_C(A, I);
    // Rprintf("Después de closure\n");

    timer.step("closure_C");

    rhs = setdifferenceC(B, A);
    timer.step("setdiff");

    if (sum(rhs) == 0) {

      // Concept
      // res2.push_back(A);//[count++] = A;

    } else {

      LHS.push_back(A);
      RHS.push_back(rhs);
      count++;

      if (count % 10 == 0) Rprintf("%u\n", count);

    }

    timer.step("push");

    if (checkInterrupt()) { // user interrupted ...

      List res = List::create(_["concepts"] = wrap(res2),
                              _["LHS"] = wrap(LHS),
                              _["RHS"] = wrap(RHS),
                              _["timer"] = timer);

      return res;

    }

  }

  List res = List::create(_["concepts"] = wrap(res2),
                          _["LHS"] = wrap(LHS),
                          _["RHS"] = wrap(RHS),
                          _["timer"] = timer);

  return res;

}


