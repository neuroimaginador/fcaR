#include <Rcpp.h>
#include <Rdefines.h>
#include <Rcpp/Benchmark/Timer.h>
// #include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;

static void chkIntFn(void *dummy) {
  R_CheckUserInterrupt();
}

// this will call the above in a top-level context so it won't longjmp-out of your context
bool checkInterrupt2() {
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
}

// [[Rcpp::export]]
S4 concat_sparse(S4 A, S4 B) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = clone<IntegerVector>(A.slot("Dim"));

  std::vector<int> bp = B.slot("p");
  std::vector<int> bi = B.slot("i");
  std::vector<double> bx = B.slot("x");
  IntegerVector bdims = B.slot("Dim");

  ai.insert(ai.end(), bi.begin(), bi.end());
  ax.insert(ax.end(), bx.begin(), bx.end());
  transform(bp.begin(), bp.end(), bp.begin(),
            bind2nd(std::plus<int>(), ap.back()));

  bp.erase(bp.begin());
  ap.insert(ap.end(), bp.begin(), bp.end());

  adims[1] += bdims[1];

  S4 res("dgCMatrix");

  res.slot("p") = ap;
  res.slot("i") = ai;
  res.slot("x") = ax;
  res.slot("Dim") = adims;

  return res;

}

// [[Rcpp::export]]
S4 intent_sparse_C(S4 A, NumericMatrix I) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  NumericVector res(I.ncol());
  int i;

  for (int c = 0; c < I.ncol(); c++) {

    double ms = 1;

    for (int r = 0; r < ai.size(); r++) {

      i = ai[r];

      double tmp = (ax[r] <= I(i, c)) ? 1.0 : I(i, c);

      if (tmp < ms) ms = tmp;

    }

    res[c] = ms;

  }

  S4 resM("dgCMatrix");

  int resp = 0;
  std::vector<int> resi;
  std::vector<double> resx;
  IntegerVector new_dims(2);
  IntegerVector new_p(2);

  for (int j = 0; j < res.size(); j++) {

    if (res[j] > 0) {

      resp++;

      resi.push_back(j);
      resx.push_back(res[j]);

    }

  }

  new_p[1] = resp;
  new_dims[0] = I.ncol();
  new_dims[1] = 1;


  resM.slot("p") = new_p;
  resM.slot("x") = resx;
  resM.slot("i") = resi;
  resM.slot("Dim") = new_dims;

  return(resM);

}

S4 convert_vector(NumericVector res) {

  S4 resM("dgCMatrix");

  int resp = 0;
  std::vector<int> resi;
  std::vector<double> resx;
  IntegerVector new_dims(2);
  IntegerVector new_p(2);

  for (int j = 0; j < res.size(); j++) {

    if (res[j] > 0) {

      resp++;

      resi.push_back(j);
      resx.push_back(res[j]);

    }

  }

  new_p[1] = resp;
  new_dims[0] = res.size();
  new_dims[1] = 1;


  resM.slot("p") = new_p;
  resM.slot("x") = resx;
  resM.slot("i") = resi;
  resM.slot("Dim") = new_dims;

  return(resM);

}

// [[Rcpp::export]]
S4 extent_sparse_C(S4 A, NumericMatrix I) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  NumericVector res(I.nrow());
  int i;

  for (int r = 0; r < I.nrow(); r++) {

    double ms = 1;

    for (int c = 0; c < ai.size(); c++) {

      i = ai[c];

      double tmp = (ax[c] <= I(r, i)) ? 1 : I(r, i);

      if (tmp < ms) ms = tmp;

    }

    res[r] = ms;

  }



  return(convert_vector(res));

}

// [[Rcpp::export]]
S4 closure_sparse_C(S4 A, NumericMatrix I) {

  return(intent_sparse_C(extent_sparse_C(A, I), I));

}

// [[Rcpp::export]]
S4 direct_sum_sparse_C(S4 A, int a_i,
                       double grade_i,
                       int imax) {

  std::vector<int> ap = A.slot("p");
  std::vector<int> ai = A.slot("i");
  std::vector<double> ax = A.slot("x");
  IntegerVector adims = A.slot("Dim");

  int resp = 0;
  std::vector<int> resi;
  std::vector<double> resx;
  IntegerVector new_dims(2);
  IntegerVector new_p(2);

  for (int i = 0; i < ai.size(); i++) {

    if (ai[i] < a_i) {

      resp++;
      resi.push_back(ai[i]);
      resx.push_back(ax[i]);

    }

  }

  resp++;
  resi.push_back(a_i);
  resx.push_back(grade_i);

  S4 the_sum("dgCMatrix");
  new_p[1] = resp;
  new_dims[0] = adims[0];
  new_dims[1] = 1;

  the_sum.slot("p") = new_p;
  the_sum.slot("x") = resx;
  the_sum.slot("i") = resi;
  the_sum.slot("Dim") = new_dims;

  return the_sum;

}

// [[Rcpp::export]]
bool is_set_preceding_sparse_C(S4 B,
                               S4 C,
                               int a_i,
                               double grade_i) {

  std::vector<int> bp = B.slot("p");
  std::vector<int> bi = B.slot("i");
  std::vector<double> bx = B.slot("x");
  IntegerVector bdims = B.slot("Dim");

  std::vector<int> cp = C.slot("p");
  std::vector<int> ci = C.slot("i");
  std::vector<double> cx = C.slot("x");
  IntegerVector cdims = C.slot("Dim");

  bool res = true;

  std::vector<int> bi_lt_a_i, ci_lt_a_i;
  std::vector<double> bx_lt_a_i, cx_lt_a_i;

  double bx_at_a_i = 0.0, cx_at_a_i = 0.0;
  for (int i = 0; i < bi.size(); i++) {

    if (bi[i] < a_i) {

      bi_lt_a_i.push_back(bi[i]);
      bx_lt_a_i.push_back(bx[i]);

    }

    if (bi[i] == a_i) {

      bx_at_a_i = bx[i];

    }

  }

  for (int i = 0; i < ci.size(); i++) {

    if (ci[i] < a_i) {

      ci_lt_a_i.push_back(ci[i]);
      cx_lt_a_i.push_back(cx[i]);

    }

    if (ci[i] == a_i) {

      cx_at_a_i = cx[i];

    }

  }

  if (cx_at_a_i != grade_i) {

    return false;

  }

  if (bx_at_a_i >= cx_at_a_i) {

    return false;

  }

  if (ci_lt_a_i.size() != bi_lt_a_i.size()) {

    return false;

  }

  for (int i = 0; i < ci_lt_a_i.size(); i++) {

    if (ci_lt_a_i[i] != bi_lt_a_i[i]) {

      return false;

    }
    if (cx_lt_a_i[i] != bx_lt_a_i[i]) {

      return false;

    }

  }

  return true;

}

// [[Rcpp::export]]
S4 empty_sparse_C(int n) {

  S4 res("dgCMatrix");
  std::vector<int> iv;
  std::vector<double> xv;
  IntegerVector d(2);
  d[0] = n;
  IntegerVector p(1);

  res.slot("p") = p;
  res.slot("i") = iv;
  res.slot("x") = xv;
  res.slot("Dim") = d;

  return(res);

}

// [[Rcpp::export]]
S4 next_closure_sparse_C(S4 A, int i, int imax,
                  NumericVector grades_set,
                  NumericMatrix I) {

  int n_grades = grades_set.size();

  S4 candB("dgCMatrix");

  for (int a_i = i - 1; i >= 0; a_i--) {

    for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

      candB = direct_sum_sparse_C(A, a_i, grades_set[grade_idx], imax);

      candB = closure_sparse_C(candB, I);

      if (is_set_preceding_sparse_C(A, candB, a_i, grades_set[grade_idx])) {

        return candB;

      }

    }

  }

  std::vector<int> dims = A.slot("Dim");
  int n_att = dims[0];

  return empty_sparse_C(n_att);

}



// [[Rcpp::export]]
double cardinal_sparse_C(S4 A) {

  std::vector<double> ax = A.slot("x");

  return(accumulate(ax.begin(), ax.end(), 0.0));

}

// [[Rcpp::export]]
List ganters_algorithm_sparse(NumericMatrix I,
                              NumericVector grades_set,
                              int n_attributes) {

  std::list <S4> res2;
  S4 empty("dgCMatrix");
  empty = empty_sparse_C(n_attributes);

  S4 A = closure_sparse_C(empty, I);

  int count = 0;

  while ((int)cardinal_sparse_C(A) < n_attributes) {

    A = next_closure_sparse_C(A, n_attributes, n_attributes, grades_set, I);

    res2.push_back(A);

    count++;

    if (count % 10 == 0) Rprintf("%u\n", count);

    if (checkInterrupt2()) { // user interrupted ...

      Rprintf("User interrupted...\n");

      return wrap(res2);

    }

  }

  return wrap(res2);

}

#include <Rcpp.h>
using namespace Rcpp;


bool populateMatches_sparse(std::vector<int> x_i, std::vector<int> x_p,
                                     std::vector<double> x,
                                     std::vector<int> y_p, std::vector<int> y_i,
                                     std::vector<double> y, int y_index, int num_rows){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  LogicalVector res(num_rows);

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

    // if(proper && (end_loc - loc == y_end_index - y_start_index)) continue;

    curr_col = y_start_index;

    while(loc < end_loc){

      if (y_i[loc] == x_i[curr_col]) {

        if (y[loc] >= x[curr_col]) {

          curr_col++;

        } else break;

      }
      if(curr_col == y_end_index) break;

      loc++;

    }


    if(curr_col == y_end_index){
      res[x_index] = true;
    }

  }

  return(res[0]);

}

// [[Rcpp::export]]
LogicalVector is_subset_one_sparse_C(S4 x, S4 y){

  std::vector<int> x_p = x.slot("p");
  std::vector<int> x_i = x.slot("i");
  std::vector<double> X = x.slot("x");

  std::vector<int> y_p = y.slot("p");
  std::vector<int> y_i = y.slot("i");
  std::vector<double> Y = y.slot("x");

  IntegerVector xdims = x.slot("Dim");
  IntegerVector ydims = y.slot("Dim");

  int x_p_length = xdims[1];

  int y_p_length = ydims[1];

  LogicalVector res(x_p_length);

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    res[y_index] = populateMatches_sparse(x_i, x_p, X, y_p, y_i, Y, y_index, y_p_length);

  }

  return res;

}

// [[Rcpp::export]]
S4 extract_col_sparse_C(S4 x, int j) {

  std::vector<int> x_p = x.slot("p");
  std::vector<int> x_i = x.slot("i");
  std::vector<double> X = x.slot("x");
  IntegerVector xdims = x.slot("Dim");

  S4 res("dgCMatrix");
  std::vector<int> ci;
  std::vector<double> cx;

  int init = x_p[j], end = x_p[j + 1];
  for (int i = init; i < end; i++) {

    ci.push_back(x_i[i]);
    cx.push_back(X[i]);

  }

  IntegerVector d(2);
  d[0] = xdims[0];
  d[1] = 1;

  IntegerVector p(2);
  p[1] = ci.size();

  res.slot("x") = cx;
  res.slot("i") = ci;
  res.slot("p") = p;
  res.slot("Dim") = d;

  return(res);

}

S4 flatten_union_sparse_C(S4 A) {

  std::vector<int> p = A.slot("p");
  std::vector<int> i = A.slot("i");
  std::vector<double> x = A.slot("x");
  IntegerVector dims = A.slot("Dim");

  int num_rows = dims[0];
  int num_cols = dims[1];

  NumericVector v(num_rows);

  for (int x_index = 0; x_index < num_cols; x_index++) {

    int start_index = p[x_index], end_index = p[x_index + 1];

    for (int j = start_index; j < end_index; j++) {

      if (x[j] > v[i[j]]) {

        v[i[j]] = x[j];

      }

    }

  }

  return convert_vector(v);

}

// [[Rcpp::export]]
S4 semantic_closure_sparse_C(S4 A, S4 LHS, S4 RHS) {

  IntegerVector adims = A.slot("Dim");
  IntegerVector idims = LHS.slot("Dim");

  int n = idims[1];

  // int n_attributes = adims[0];
  S4 res = clone(A);

  LogicalVector subsets(n);
  LogicalVector black_list(n);
  int idx;

  for (int i = 0; i < n; i++) {

    black_list[i] = true;

  }

  subsets = is_subset_one_sparse_C(LHS, A);

  while (sum(subsets) > 0) {

    idx = 0;

    for (int j = 0; j < n; j++) {

      if (subsets[j]) {

        res = concat_sparse(res, extract_col_sparse_C(RHS, j));
        black_list[idx] = false;

      }

      idx++;

    }

    res = flatten_union_sparse_C(res);
    subsets = is_subset_one_sparse_C(LHS, res) & black_list;

  }

  return res;

}

// [[Rcpp::export]]
S4 next_closure_implications_sparse_C(S4 A, int i,
                               int imax,
                               List grades_set,
                               S4 LHS,
                               S4 RHS) {

  int n_grades = 0;
  S4 candB("dgCMatrix");
  NumericVector gr;

  for (int a_i = i - 1; i >= 0; a_i--) {

    gr = grades_set[a_i];
    n_grades = gr.size();

    for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

      candB = direct_sum_sparse_C(A, a_i, gr[grade_idx], imax);

      candB = semantic_closure_sparse_C(candB, LHS, RHS);

      if (is_set_preceding_sparse_C(A, candB, a_i, gr[grade_idx])) {

        return candB;

      }

    }

  }

  std::vector<int> dims = A.slot("Dim");
  int n_att = dims[0];

  return empty_sparse_C(n_att);

}

// [[Rcpp::export]]
S4 setdifference_sparse_C(S4 x, S4 y) {

  std::vector<int> x_p = x.slot("p");
  std::vector<int> x_i = x.slot("i");
  std::vector<double> X = x.slot("x");

  std::vector<int> y_p = y.slot("p");
  std::vector<int> y_i = y.slot("i");
  std::vector<double> Y = y.slot("x");

  IntegerVector xdims = x.slot("Dim");
  IntegerVector ydims = y.slot("Dim");

  S4 res("dgCMatrix");
  std::vector<int> resp(2);
  std::vector<int> resi;
  std::vector<double> resx;
  IntegerVector resdims = xdims;

  for (int i = 0; i < x_i.size(); i++) {

    bool add = true;

    for (int j = 0; j < y_i.size(); j++) {

      if (x_i[i] == y_i[j]) {

        if (Y[j] >= X[i]) {

          add = false;
          break;

        }

      }

    }

    if (add) {

      resp[1] = resp[1] + 1;
      resi.push_back(x_i[i]);
      resx.push_back(X[i]);

    }

  }

  res.slot("p") = resp;
  res.slot("x") = resx;
  res.slot("i") = resi;
  res.slot("Dim") = resdims;

  return res;

}

// [[Rcpp::export]]
List ganters_algorithm_implications_sparse_C(NumericMatrix I,
                                             List grades_set,
                                             int n_attributes) {

  Timer timer;

  timer.step("start");

  S4 concepts = empty_sparse_C(n_attributes);
  S4 LHS = empty_sparse_C(n_attributes);
  S4 RHS = empty_sparse_C(n_attributes);

  S4 empty = empty_sparse_C(n_attributes);
  S4 B = empty_sparse_C(n_attributes);
  S4 rhs = empty_sparse_C(n_attributes);


  S4 A = closure_sparse_C(empty, I);

  if (cardinal_sparse_C(A) > 0) {

    LHS = concat_sparse(LHS, empty);
    RHS = concat_sparse(RHS, A);

  } else {

    concepts = clone(A);

  }

  int count = 0;

  while (cardinal_sparse_C(A) < n_attributes) {

    timer.step("in");
    A = next_closure_implications_sparse_C(A, n_attributes, n_attributes,
                                           grades_set, LHS, RHS);

    timer.step("next_closure");
    // Rprintf("Antes de closure\n");
    B = closure_sparse_C(A, I);
    // Rprintf("Después de closure\n");

    timer.step("closure_C");

    rhs = setdifference_sparse_C(B, A);
    timer.step("setdiff");

    if (cardinal_sparse_C(rhs) == 0) {

      // Concept
      concepts = concat_sparse(concepts, A);

    } else {

      LHS = concat_sparse(LHS, A);
      RHS = concat_sparse(RHS, rhs);
      count++;

      if (count % 10 == 0) Rprintf("%u\n", count);

    }

    timer.step("push");

    if (checkInterrupt2()) { // user interrupted ...

      List res = List::create(_["concepts"] = concepts,
                              _["LHS"] = LHS,
                              _["RHS"] = RHS,
                              _["timer"] = timer);

      return res;

    }

    // Rf_PrintValue(timer);

  }

  List res = List::create(_["concepts"] = concepts,
                          _["LHS"] = LHS,
                          _["RHS"] = RHS,
                          _["timer"] = timer);

  return res;

}

//
// double setdiffC(double x, double y) {
//
//   if (y >= x) return(0);
//
//   return(x);
//
// }
//
// NumericVector setunionC(NumericVector x, NumericVector y) {
//
//   int n = x.size();
//   NumericVector res = clone(x);
//
//   for (int i = 0; i < n; i++) {
//
//     if (y[i] > x[i]) res[i] = y[i];
//
//   }
//
//   return res;
//
// }
//



//

//
// static void chkIntFn(void *dummy) {
//   R_CheckUserInterrupt();
// }
//
// // this will call the above in a top-level context so it won't longjmp-out of your context
// bool checkInterrupt() {
//   return (R_ToplevelExec(chkIntFn, NULL) == FALSE);
// }
//
//

//
//
