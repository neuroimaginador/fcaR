/*
 C implementation of sparse matrix subset
 Author: Ian Johnson
 */

// #include <R.h>
// #include <Rdefines.h>
#include <Rcpp.h>
using namespace Rcpp;


void populateMatches(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

    curr_col = y_start_index;

    if (curr_col >= y_end_index) continue;

    while(loc < end_loc){

      if (y_i[loc] == x_i[curr_col]) {

        if (y[loc] >= x[curr_col]) {

          curr_col++;

        } else break;

      }
      if(curr_col >= y_end_index) break;

      loc++;

    }


    if(curr_col == y_end_index){
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}

void populateMatchesEqual(int* matches_for_y, int* x_i, int* x_p, double* x, int* y_p, int* y_i, double* y, int y_index, int num_rows, int proper){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1];

    // To be equal, they have to have the same number of nnz rows.
    if (end_loc - loc != y_end_index - y_start_index) continue;

    bool all_equal = true;

    for (int idx = 0; idx < end_loc - loc; idx++) {

      if (x_i[y_start_index + idx] != y_i[loc + idx]) {

        all_equal = false;
        break;

      }

      if (x[y_start_index + idx] != y[loc + idx]) {

        all_equal = false;
        break;

      }

    }

    if (all_equal){
      matches_for_y[num_matches++] = x_index;
    }

  }

  matches_for_y[num_matches] = -1;

}

// [[Rcpp::export]]
IntegerVector self_intersection_C(IntegerVector x_i,
                                  IntegerVector x_p,
                                  IntegerVector y_i,
                                  IntegerVector y_p) {

  int num_rows = y_p.size() - 1;
  IntegerVector res(num_rows);

  for (int i = 0; i < num_rows; i++) {

    int y_start_index = x_p[i], y_end_index = x_p[i + 1];
    int loc = y_p[i], end_loc = y_p[i + 1];

    // int curr_col = y_start_index;

    bool has_intersection = false;
    bool out = false;

    for (int i1 = y_start_index; i1 < y_end_index; i1++) {

      for (int i2 = loc; i2 < end_loc; i2++) {

        if (y_i[i2] == x_i[i1]) {

          has_intersection = true;
          out = true;
          break;

        }

      }

      if (out) break;

    }

    if (has_intersection) res[i] = 1;

  }

  return(res);

}

void populateMatchesIntersect(int* matches_for_y, int* x_i, int* x_p, int* y_p, int* y_i, int y_index, int num_rows){

  int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

  int num_matches = 0;

  for(int x_index = 0; x_index < num_rows; x_index++){

    int loc = y_p[x_index], end_loc = y_p[x_index+1];

    // int curr_col = y_start_index;

    bool has_intersection = false;
    bool out = false;

    for (int i1 = y_start_index; i1 < y_end_index; i1++) {

      for (int i2 = loc; i2 < end_loc; i2++) {

        if (y_i[i2] == x_i[i1]) {

          has_intersection = true;
          out = true;
          break;

        }

      }

      if (out) break;

    }

    if (has_intersection) {

      matches_for_y[num_matches++] = x_index;

    }

  }

  matches_for_y[num_matches] = -1;

}

int copyMatches(int* y_matches, int** output_i, int* output_i_length, int* output_i_last){

  int index = 0;

  while(y_matches[index] != -1){

    if(*output_i_last == *output_i_length - 1){
      int* tmp = (int*)malloc(2*(*output_i_length) * sizeof(int));
      memcpy(tmp, *output_i, *output_i_length*sizeof(int));
      *output_i_length *= 2;
      free(*output_i);
      *output_i = tmp;
    }

    (*output_i)[++(*output_i_last)] = y_matches[index++];

  }

  return index;

}

// [[Rcpp::export]]
SEXP is_subset_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  double* x = REAL(X);
  double* y = REAL(Y);

  int proper = LOGICAL(PROPER)[0];

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  /* MFH: unused
   * int y_i_max    = INTEGER(Y_DIM)[0];
   */

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatches(y_matches, x_i, x_p, x, y_p, y_i, y, y_index, y_p_length, proper);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}

// [[Rcpp::export]]
SEXP intersects_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all intersections in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatchesIntersect(y_matches, x_i, x_p, y_p, y_i, y_index, y_p_length);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}

// [[Rcpp::export]]
SEXP is_equal_set_C(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP X, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP Y, SEXP PROPER, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  double* x = REAL(X);
  double* y = REAL(Y);

  int proper = LOGICAL(PROPER)[0];

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];

  /* MFH: unused
   * int y_i_max    = INTEGER(Y_DIM)[0];
   */

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = (int*)malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = (int*)malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatchesEqual(y_matches, x_i, x_p, x, y_p, y_i, y, y_index, y_p_length, proper);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = Rf_allocVector(INTSXP, output_i_last+1);
  for(int i = 0; i < output_i_last+1; i++){
    INTEGER(OUT_I)[i] = output_i[i];
  }

  free(output_i);

  return OUT_I;

}

// From here, own code

// [[Rcpp::export]]
IntegerVector which_at_col(IntegerVector x_i, IntegerVector x_p, int col) {

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
NumericVector flatten_sparse_C(IntegerVector p,
                               IntegerVector i,
                               NumericVector x,
                               NumericVector dims) {

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

  return v;

}


