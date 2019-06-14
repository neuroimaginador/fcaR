#include <Rcpp.h>
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

// [[Rcpp::export]]
IntegerVector which_at_col_C(IntegerVector x_i, IntegerVector x_p, int col) {

  col--;
  if (x_p[col + 1] == x_p[col]) return 0;

  int len = x_p[col + 1] - x_p[col];

  IntegerVector res(len);

  for (int i = 0; i < len; i++) {

    res[i] = x_i[x_p[col] + i] + 1;

  }

  return res;

}

// [[Rcpp::export]]
bool pre_condition(IntegerVector x_i, IntegerVector y_i) {

  if (x_i.size() == 0) return true;

  if (!std::includes(y_i.begin(), y_i.end(),
                     x_i.begin(), x_i.end())) return false;

  // X subset of Y
  // if ((min(y_i) > max(x_i)) | (max(y_i) < min(x_i))) return false;
  //
  // if ((max(y_i) > max(x_i)) | (min(y_i) < min(x_i))) return false;

  return true;

}

// [[Rcpp::export]]
bool is_pre_condition(IntegerVector x_i, IntegerVector x_p,
                      IntegerVector y_i, IntegerVector y_p) {

  IntegerVector my_xi, my_yi;
  bool res;

  int ncolsx = x_p.size(), ncolsy = y_p.size();

  for (int i = 0; i < ncolsx; i++) {

    my_xi = which_at_col_C(x_i, x_p, i + 1);

    for (int j = 0; j < ncolsy; j++) {

      my_yi = which_at_col_C(y_i, y_p, i + 1);

      res = pre_condition(my_xi, my_yi);

    }

  }

  return res;

}

// [[Rcpp::export]]
bool is_subset_individual(IntegerVector x_i, NumericVector x,
                          IntegerVector y_i, NumericVector y,
                          int len) {

  if (!pre_condition(x_i, y_i)) return false;

  IntegerVector idx = intersect(x_i, y_i);

  bool res = true;

  for (int i = 0; i < idx.size(); i++) {

    if (x[idx[i]] > y[idx[i]]) {

      res = false;
      break;

    }


  }

  return res;


  // int loc = y_i[0], end_loc = y_i[y_i.size() - 1], curr_col;
  //
  // curr_col = x_i[0];
  //
  // while(loc < end_loc){
  //
  //   if (y_i[loc] == x_i[curr_col]) {
  //
  //     if (y[loc] <= x[curr_col]) {
  //
  //       curr_col++;
  //
  //     } else break;
  //
  //   }
  //   if(curr_col == x_i.size() - 1) break;
  //
  //   loc++;
  //
  // }
  //
  //
  // if (curr_col == x_i.size() - 1) {
  //
  //   return true;
  //
  // }
  //
  // return false;

}


//
//
//
// std::vector<int> c_which(NumericVector x, double args) {
//   std::vector<int> ind = x > args;
//   std::vector<int> out(ind);
//   std::vector<int>::iterator it;
//   int j = 0;
//   it = std::find(ind.begin(), ind.end(), 1);
//   while(it++ != ind.end()){
//     out[j++] = it - ind.begin();
//     it = std::find(it, ind.end(), 1);
//   }
//   out.resize(j);
//   return out;
// }
//
// // [[Rcpp::export]]
// List compare_vector(NumericVector x) {
//
//   int n = x.size();
//
//   List res;
//
//   for (int i = 0; i < n ; i++) {
//
//     res[i] = c_which(x, x[i]);
//
//     // for (int j = i; j < n; j++) {
//     //
//     //   if (x[i] <= x[j]) res.push_back(j);
//     //
//     // }
//
//   }
//
//   return res;
//
// }

void populateMatchesSubset2(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for (int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

    // For X to be subset of Y, must have less nnz rows
    if (end_loc - loc < y_end_index - y_start_index) continue;

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
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}
