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
List compare_elements_first(NumericVector x) {

  int my_size = x.size();

  std::vector<int> idx_i(my_size), idx_j(my_size);

  int count = 0;
  int max_size = my_size;

  for (int i = 0; i < my_size - 1; i++) {

    for (int j = i + 1; j < my_size; j++) {

      if (x[i] <= x[j]) {

        if (count >= max_size) {

          // Reserve more memory
          idx_i.resize(max_size + my_size);
          idx_j.resize(max_size + my_size);

          max_size += my_size;

        }

        idx_i[count] = i + 1;
        idx_j[count] = j + 1;

        count++;

      }

      if (x[j] <= x[i]) {

        if (count >= max_size) {

          // Reserve more memory
          idx_i.resize(max_size + my_size);
          idx_j.resize(max_size + my_size);

          max_size += my_size;

        }

        idx_j[count] = i + 1;
        idx_i[count] = j + 1;

        count++;

      }

    }

  }

  List ret;
  ret["i"] = idx_i;
  ret["j"] = idx_j;

  return ret;

}

// [[Rcpp::export]]
List compare_elements_other(NumericVector x,
                            IntegerVector idx_i,
                            IntegerVector idx_j) {

  int my_size = idx_i.size();

  std::vector<int> idx_i2, idx_j2;

  for (int i = 0; i < my_size; i++) {

    if (x[idx_i[i] - 1] <= x[idx_j[i] - 1]) {

      idx_i2.push_back(idx_i[i]);
      idx_j2.push_back(idx_j[i]);

    }

  }

  List ret;
  ret["i"] = idx_i2;
  ret["j"] = idx_j2;

  return ret;

}


// [[Rcpp::export]]
List compare_equality_other(NumericVector x,
                            IntegerVector idx_i,
                            IntegerVector idx_j) {

  int my_size = idx_i.size();

  std::vector<int> idx_i2, idx_j2;

  for (int i = 0; i < my_size; i++) {

    if (x[idx_i[i] - 1] == x[idx_j[i] - 1]) {

      idx_i2.push_back(idx_i[i]);
      idx_j2.push_back(idx_j[i]);

    }

  }

  List ret;
  ret["i"] = idx_i2;
  ret["j"] = idx_j2;

  return ret;

}

