#include <Rcpp.h>
using namespace Rcpp;

#define arrayDup(DST, SRC, LEN)                                \
{ size_t TMPSZ = sizeof(*(SRC)) * (LEN);                       \
  if ( ((DST) = (typeof(DST))malloc(TMPSZ)) != NULL )          \
    memcpy((DST), (SRC), TMPSZ); }                             \


  typedef struct {
    int *array;
    size_t used;
    size_t size;
  } IntArray;

    void initArray(IntArray *a, size_t initialSize) {
      a->array = (int *)malloc(initialSize * sizeof(int));
      a->used = 0;
      a->size = initialSize;
    }

    void printArray(IntArray a) {

      Rprintf("Array:\n");
      Rprintf("Used / Size = %u / %u\n", a.used, a.size);
      for (int i = 0; i < a.used; i++) {

        Rprintf("%d ", a.array[i]);

      }

      Rprintf("\n");
    }

    void insertArray(IntArray *a, int element) {
      // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
      // Therefore a->used can go up to a->size
      if (a->used == a->size) {
        a->size *= 2;
        a->array = (int *)realloc(a->array, a->size * sizeof(int));
      }
      a->array[a->used++] = element;
    }

    void freeArray(IntArray *a) {
      free(a->array);
      a->array = NULL;
      a->used = a->size = 0;
    }

    typedef struct {
      double *array;
      size_t used;
      size_t size;
    } DoubleArray;

    void initArray(DoubleArray *a, size_t initialSize) {
      a->array = (double *)malloc(initialSize * sizeof(double));
      a->used = 0;
      a->size = initialSize;
    }

    void reinitArray(DoubleArray *a) {

      a->used = 0;

    }
    void reinitArray(IntArray *a) {

      a->used = 0;

    }

    void printArray(DoubleArray a) {

      Rprintf("Array:\n");
      Rprintf("Used / Size = %u / %u\n", a.used, a.size);
      for (int i = 0; i < a.used; i++) {

        Rprintf("%f ", a.array[i]);

      }

      Rprintf("\n");
    }

    void insertArray(DoubleArray *a, double element) {
      // a->used is the number of used entries, because a->array[a->used++] updates a->used only *after* the array has been accessed.
      // Therefore a->used can go up to a->size
      if (a->used == a->size) {
        a->size *= 2;
        a->array = (double *)realloc(a->array, a->size * sizeof(double));
      }
      a->array[a->used++] = element;
    }

    void freeArray(DoubleArray *a) {
      free(a->array);
      a->array = NULL;
      a->used = a->size = 0;
    }

    typedef struct {
      IntArray p;
      IntArray i;
      DoubleArray x;
      size_t length;
    } SparseVector;

    void initVector(SparseVector *a, size_t initialSize) {

      initArray(&(a->p), initialSize);
      initArray(&(a->i), initialSize);
      initArray(&(a->x), initialSize);
      a->length = initialSize;

    }

    void reinitVector(SparseVector *a) {

      reinitArray(&(a->i));
      reinitArray(&(a->x));

    }

    void freeVector(SparseVector *a) {

      freeArray(&(a->i));
      freeArray(&(a->p));
      freeArray(&(a->x));

      a->length = 0;

    }

    void printVector(SparseVector A, Rcpp::StringVector attrs) {

      Rprintf("{");

      for (int i = 0; i < A.i.used - 1; i++) {

        if (A.x.array[i] < 1) {

          Rcout << attrs[A.i.array[i]] << " [" << A.x.array[i] << "], ";

        } else {

          Rcout << attrs[A.i.array[i]] << ", ";

        }

      }

      int end = A.i.used - 1;

      if (end >= 0) {

        if (A.x.array[end] < 1) {

          Rcout << attrs[A.i.array[end]] << " [" << A.x.array[end] << "]";

        } else {

          Rcout << attrs[A.i.array[end]];

        }

      }

      Rprintf("}");

    }

    void printImpl(SparseVector A,
                   SparseVector B,
                   Rcpp::StringVector attrs) {

      printVector(A, attrs);
      Rprintf(" -> ");
      printVector(B, attrs);
      Rprintf("\n");


    }

    void assignUsed(IntArray *a, const size_t n) {

      a->used = n;

    }

    void assignUsed(DoubleArray *a, const size_t n) {

      a->used = n;

    }

    void cloneVector(SparseVector *a, SparseVector b) {

      freeVector(a);
      initVector(a, b.length);

      if (b.i.used > 0) {

        std::copy(&b.i.array[0], &b.i.array[b.i.used], a->i.array);
        std::copy(&b.x.array[0], &b.x.array[b.i.used], a->x.array);

        //    memcpy(a->i.array, b.i.array, b.i.used * sizeof(int));
        //    memcpy(a->x.array, b.x.array, b.i.used * sizeof(double));

      }

      assignUsed(&(a->i), b.i.used);
      assignUsed(&(a->x), b.x.used);


    }

    void add_column(SparseVector *a, SparseVector b) {

      if (a->p.used > 0) {

        int last_p = a->p.array[a->p.used - 1];

        for (int i = 0; i < b.i.used; i++) {

          insertArray(&(a->i), b.i.array[i]);
          insertArray(&(a->x), b.x.array[i]);

        }

        insertArray(&(a->p), last_p + b.i.used);

      } else {

        for (int i = 0; i < b.i.used; i++) {

          insertArray(&(a->i), b.i.array[i]);
          insertArray(&(a->x), b.x.array[i]);

        }

        insertArray(&(a->p), 0);
        insertArray(&(a->p), b.i.used);

      }


    }

    SparseVector S4toSparse(S4 A) {

      std::vector<int> ap = A.slot("p");
      std::vector<int> ai = A.slot("i");
      std::vector<double> ax = A.slot("x");
      IntegerVector adims = A.slot("Dim");

      SparseVector V;
      initVector(&V, adims[0]);

      for (int i = 0; i < ai.size(); i++) {

        insertArray(&(V.i), ai[i]);
        insertArray(&(V.x), ax[i]);

      }
      insertArray(&(V.p), 0);

      if (V.i.used > 0) {

        insertArray(&(V.p), V.i.used);

      } else {

        insertArray(&(V.p), 0);

      }

      return V;

    }

    S4 SparseToS4(SparseVector V) {

      S4 res("dgCMatrix");

      std::vector<int> i;
      std::vector<double> x;
      IntegerVector dims(2);
      std::vector<int> p;

      for (int j = 0; j < V.i.used; j++) {

        i.push_back(V.i.array[j]);
        x.push_back(V.x.array[j]);

      }

      p.push_back(0);

      if (V.p.used > 0) {

        for (int j = 0; j < V.p.used; j++) {

          p.push_back(V.p.array[j]);

        }

      }

      dims[0] = V.length;
      dims[1] = V.p.used;

      res.slot("x") = x;
      res.slot("i") = i;
      res.slot("Dim") = dims;
      res.slot("p") = p;

      return(res);

    }

    struct ImplicationTree {

      IntArray COUNT;
      DoubleArray CARD;
      DoubleArray DEGREE[55];
      IntArray LIST[55];
      int n_implications;
      int n_attributes;

    };

    static void
      _finalizer(SEXP ext)
      {
        struct ImplicationTree *ptr = (struct ImplicationTree*) R_ExternalPtrAddr(ext);
        Rprintf("Destroying pointer.\n");
        freeArray(&(ptr->CARD));
        freeArray(&(ptr->COUNT));
        free(ptr->DEGREE);
        free(ptr->LIST);
        Free(ptr);

      }


    void initImplicationTree(struct ImplicationTree *t, int n_attributes) {

      initArray(&(t->CARD), n_attributes);
      initArray(&(t->COUNT), n_attributes);
      t->n_implications = 0;
      t->n_attributes = n_attributes;

      // Rprintf("Number of attributes: %u\n", t->n_attributes);
      // Rprintf("Number of implications: %d\n", t->n_implications);

      for (int i = 0; i < n_attributes; i++) {

        initArray(&(t->LIST[i]), n_attributes);
        initArray(&(t->DEGREE[i]), n_attributes);

      }

    }

    // [[Rcpp::export]]
    void printImplicationTree(SEXP ext) {

      struct ImplicationTree *tree = (struct ImplicationTree*) R_ExternalPtrAddr(ext);

      Rprintf("ImplicationTree\n");
      Rprintf("Number of attributes: %u\n", tree->n_attributes);
      Rprintf("Number of implications: %d\n", tree->n_implications);

      printArray(tree->CARD);
      printArray(tree->COUNT);

    }

    // [[Rcpp::export]]
    SEXP createImplicationTree(int n_attributes) {

      struct ImplicationTree *t = Calloc(1, struct ImplicationTree);
      initImplicationTree(t, n_attributes);

      SEXP ext = PROTECT(R_MakeExternalPtr(t, R_NilValue, R_NilValue));
      R_RegisterCFinalizerEx(ext, _finalizer, TRUE);
      UNPROTECT(1);

      return ext;

    }

    void addImplicationToTree(struct ImplicationTree *t, SparseVector A) {

      int new_idx = t->n_implications;

      // Rprintf("**************Before inserting\n");
      insertArray(&(t->CARD), 0);
      insertArray(&(t->COUNT), 0);

      t->n_implications = t->n_implications + 1;

      // Rprintf("Before for**********************\n");
      for (int i = 0; i < A.i.used; i++) {

        // Rprintf("A[%u] = %u, used = %u, size = %u\n", i, A.i.array[i], t->LIST[A.i.array[i]].used, t->LIST[A.i.array[i]].size);
        // printArray(t->LIST[A.i.array[i]]);
        insertArray(&(t->LIST[A.i.array[i]]), new_idx);

        insertArray(&(t->DEGREE[A.i.array[i]]), A.x.array[i]);

        (t->CARD).array[new_idx] = (t->CARD).array[new_idx] + A.x.array[i];
        (t->COUNT).array[new_idx] = (t->COUNT).array[new_idx] + 1;

      }

      // Rprintf("Added with CARD = %f, COUNT = %u\n", (t->CARD).array[new_idx], (t->COUNT).array[new_idx]);

    }

    // [[Rcpp::export]]
    void addImplicationToTree_XPtr(SEXP ext, S4 A) {

      // printImplicationTree(ext);

      SparseVector S = S4toSparse(A);
      struct ImplicationTree *tree = (struct ImplicationTree*) R_ExternalPtrAddr(ext);
      addImplicationToTree(tree, S);

    }

    LogicalVector is_subset_tree_struct (SparseVector A,
                                         const struct ImplicationTree t) {

      LogicalVector res(t.n_implications);

      int* counts = (int*)malloc(t.COUNT.used * sizeof(int));

      // memcpy(counts, t.COUNT.array, t.COUNT.used * sizeof(int));
      arrayDup(counts, t.COUNT.array, t.COUNT.used);

      for (int i = 0; i < A.i.used; i++) {

        int y = A.i.array[i];
        double a = A.x.array[i];

        for (int j = 0; j < t.DEGREE[y].used; j++) {

          if (t.DEGREE[y].array[j] <= a) {

            counts[t.LIST[y].array[j]] = counts[t.LIST[y].array[j]] - 1;

            if (counts[t.LIST[y].array[j]] == 0) {

              res[t.LIST[y].array[j]] = true;

            }

          }

        }

      }

      free(counts);

      return res;

    }


    void is_subset_tree_struct (SparseVector A,
                                const struct ImplicationTree t,
                                bool *res) {

      int* counts = (int*)malloc(t.COUNT.used * sizeof(int));

      // memcpy(counts, t.COUNT.array, t.COUNT.used * sizeof(int));
      //
      for (int i = 0; i < t.COUNT.used; i++) {

        counts[i] = t.COUNT.array[i];

      }
      // arrayDup(counts, t.COUNT.array, t.COUNT.used);

      for (int i = 0; i < A.i.used; i++) {

        int y = A.i.array[i];
        double a = A.x.array[i];

        for (int j = 0; j < t.DEGREE[y].used; j++) {

          if (t.DEGREE[y].array[j] <= a) {

            counts[t.LIST[y].array[j]] = counts[t.LIST[y].array[j]] - 1;

            if (counts[t.LIST[y].array[j]] == 0) {

              res[t.LIST[y].array[j]] = true;

            }

          }

        }

      }

      free(counts);

    }

    void is_subset_tree_struct3(SparseVector A,
                                const struct ImplicationTree t,
                                IntArray *res,
                                bool* black_list) {

      reinitArray(res);

      if (t.COUNT.used > 0) {

        int* counts = (int*)malloc(t.COUNT.used * sizeof(int));

        std::copy(&t.COUNT.array[0], &t.COUNT.array[t.COUNT.used], counts);

        // memcpy(counts, t.COUNT.array, t.COUNT.used * sizeof(int));
        // //
        // for (int i = 0; i < t.COUNT.used; i++) {
        //
        //   counts[i] = t.COUNT.array[i];
        //
        // }
        // arrayDup(counts, t.COUNT.array, t.COUNT.used);

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

    // [[Rcpp::export]]
    LogicalVector is_subset_tree_XPtr(SEXP ext, S4 A) {

      SparseVector S = S4toSparse(A);
      struct ImplicationTree *tree = (struct ImplicationTree*) R_ExternalPtrAddr(ext);

      return is_subset_tree_struct (S, *tree);

    }


    SparseVector setunion_struct(SparseVector x,
                                 SparseVector y) {

      SparseVector res;

      int j = 0;

      for (int i = 0; i < x.i.used; i++) {

        while ((j < y.i.used) & (y.i.array[j] < x.i.array[i])) {

          insertArray(&(res.i), y.i.array[j]);
          insertArray(&(res.x), y.x.array[j]);
          j++;

        }

        if (y.i.array[j] == x.i.array[i]) {

          if (x.x.array[i] > y.x.array[j]) {

            insertArray(&(res.i), x.i.array[j]);
            insertArray(&(res.x), x.x.array[j]);
            j++;

          } else {

            insertArray(&(res.i), y.i.array[j]);
            insertArray(&(res.x), y.x.array[j]);
            j++;

          }

        } else {

          insertArray(&(res.i), x.i.array[j]);
          insertArray(&(res.x), x.x.array[j]);

        }

      }

      return res;

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

    // [[Rcpp::export]]
    S4 compute_intent_S4 (S4 A, NumericMatrix I) {

      SparseVector V = S4toSparse(A);

      SparseVector R = compute_intent_struct(V, I);

      return SparseToS4(R);

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

    // [[Rcpp::export]]
    S4 compute_extent_S4 (S4 A, NumericMatrix I) {

      SparseVector V = S4toSparse(A);

      SparseVector R = compute_extent_struct(V, I);

      return(SparseToS4(R));

    }

    SparseVector compute_closure_struct (SparseVector V,
                                         NumericMatrix I) {

      SparseVector A = compute_extent_struct(V, I);
      SparseVector B = compute_intent_struct(A, I);

      return B;

    }

    // [[Rcpp::export]]
    S4 compute_closure_S4 (S4 A, NumericMatrix I) {

      SparseVector V = S4toSparse(A);

      SparseVector R = compute_closure_struct(V, I);

      return(SparseToS4(R));

    }

    SparseVector direct_sum_struct(SparseVector A,
                                   int a_i,
                                   double grade_i,
                                   int imax) {

      SparseVector res;
      initVector(&res, A.length);

      int resp = 0;

      for (int i = 0; i < A.i.used; i++) {

        if (A.i.array[i] < a_i) {

          resp++;
          insertArray(&(res.i), A.i.array[i]);
          insertArray(&(res.x), A.x.array[i]);

        }

        if (A.i.array[i] >= a_i) {

          break;

        }

      }

      resp++;
      insertArray(&(res.i), a_i);
      insertArray(&(res.x), grade_i);

      return res;

    }

    void direct_sum_struct3(SparseVector A,
                            int a_i,
                            double grade_i,
                            int imax,
                            SparseVector *res) {

      // SparseVector res;
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

      // for (int i = 0; i < A.i.used; i++) {
      //
      //   if (A.i.array[i] < a_i) {
      //
      //     resp++;
      //     insertArray(&(res->i), A.i.array[i]);
      //     insertArray(&(res->x), A.x.array[i]);
      //
      //   }
      //
      //   if (A.i.array[i] >= a_i) {
      //
      //     break;
      //
      //   }
      //
      // }
      //
      // resp++;
      insertArray(&(res->i), a_i);
      insertArray(&(res->x), grade_i);

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

      // Rprintf("B=\n");
      // printArray(bi_lt_a_i);
      // printArray(bx_lt_a_i);
      //
      // Rprintf("C=\n");
      // printArray(ci_lt_a_i);
      // printArray(cx_lt_a_i);


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

    bool is_subset_one_struct (SparseVector LHS, SparseVector A) {

      int y_start_index = 0;
      int y_end_index = LHS.i.used;

      int loc = 0;
      int end_loc = A.i.used;
      int curr_col;

      // if(proper && (end_loc - loc == y_end_index - y_start_index)) continue;

      curr_col = y_start_index;

      while(loc < end_loc){

        if (A.i.array[loc] == LHS.i.array[curr_col]) {

          if (A.x.array[loc] >= LHS.x.array[curr_col]) {

            curr_col++;

          } else break;

        }
        if (curr_col == y_end_index) break;

        loc++;

      }


      if (curr_col == y_end_index) {

        return true;

      } else {

        return false;

      }

    }

    // [[Rcpp::export]]
    LogicalVector is_subset_tree_C (ListOf<IntegerVector> LIST,
                                    ListOf<NumericVector> DEGREE,
                                    IntegerVector COUNT,
                                    int n_implications,
                                    S4 S) {

      IntegerVector attrs = S.slot("i");
      NumericVector values = S.slot("x");

      LogicalVector res(n_implications);

      if (attrs.size() > 0) {

        for (int i = 0; i < attrs.size(); i++) {

          int y = attrs[i];
          double a = values[i];

          for (int j = 0; j < DEGREE[y].size(); j++) {

            if (DEGREE[y][j] <= a) {

              COUNT[LIST[y][j] - 1] = COUNT[LIST[y][j] - 1] - 1;

              if (COUNT[LIST[y][j] - 1] == 0) {

                res[LIST[y][j]] = true;

              }

            }

          }

        }

        for (int i = 0; i < n_implications; i++) {

          if (COUNT[i] == 0) {

            res[i] = true;

          } else {

            res[i] = false;

          }
        }

      }

      return res;

    }

    SparseVector semantic_closure_C(SparseVector A,
                                    std::list<SparseVector> LHS,
                                    std::list<SparseVector> RHS) {

      int n = LHS.size();

      int n_attributes = A.length;
      SparseVector res;

      IntArray I;
      initArray(&I, n_attributes);
      DoubleArray X;
      initArray(&X, n_attributes);


      initVector(&res, n_attributes);
      for (int i = 0; i < A.i.used; i++) {

        insertArray(&(res.i), A.i.array[i]);
        insertArray(&(res.x), A.x.array[i]);

      }

      LogicalVector subsets(n);
      LogicalVector black_list(n);
      int idx;

      for (int i = 0; i < n; i++) {

        black_list[i] = true;

      }

      std::list<SparseVector>::iterator it, it2;

      idx = 0;
      for (it2 = LHS.begin(); it2 != LHS.end(); ++it2 ) {

        subsets[idx++] = is_subset_one_struct((*it2), A);

      }

      while (sum(subsets) > 0) {

        idx = 0;

        for (it = RHS.begin(); it != RHS.end(); ++it ) {

          if (subsets[idx]) {

            res = setunion_struct(res, (*it));
            // setunion_struct(res, (*it), &I, &X);
            // res.i = I;
            // res.x = X;
            //
            // I.used = 0;
            // X.used = 0;
            //
            // black_list[idx] = false;

          }

          idx++;

        }

        idx = 0;
        for (it2 = LHS.begin(); it2 != LHS.end(); ++it2 ) {

          subsets[idx++] = is_subset_one_struct((*it2), A);

        }


        subsets = subsets & black_list;

      }

      return res;

    }


    SparseVector semantic_closure_tree (SparseVector A,
                                        ImplicationTree t,
                                        std::list<SparseVector> RHS) {

      int n = RHS.size();

      int n_attributes = A.length;

      SparseVector res;
      SparseVector res2;

      initVector(&res, n_attributes);
      initVector(&res2, n_attributes);

      for (int i = 0; i < A.i.used; i++) {

        insertArray(&(res.i), A.i.array[i]);
        insertArray(&(res.x), A.x.array[i]);

      }

      bool* subsets = (bool*)malloc(n * sizeof(bool));
      bool* black_list = (bool*)malloc(n * sizeof(bool));
      int idx;

      for (int i = 0; i < n; i++) {

        black_list[i] = true;
        subsets[i] = false;

      }

      std::list<SparseVector>::iterator it, it2;

      is_subset_tree_struct(A, t, subsets);

      int suma = 0;

      for (int i = 0; i < n; i++) {

        if (subsets[i]) {

          suma++;

        }

      }

      while (suma > 0) {

        idx = 0;

        for (it = RHS.begin(); it != RHS.end(); ++it ) {

          if (subsets[idx]) {

            setunion_struct2(res, (*it), &res2);

            cloneVector(&res, res2);

            reinitVector(&res2);

            black_list[idx] = false;

          }

          idx++;

        }

        for (int i = 0; i < n; i++) {

          subsets[i] = false;

        }


        is_subset_tree_struct(res, t, subsets);

        suma = 0;

        for (int i = 0; i < n; i++) {

          if (subsets[i]) {

            if (black_list[i]) {

              suma++;

            } else {

              subsets[i] = false;

            }

          }

        }


      }

      freeVector(&res2);

      free(subsets);

      free(black_list);

      return res;

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

    SparseVector semantic_closure_tree3(SparseVector A,
                                        ImplicationTree t,
                                        SparseVector LHS,
                                        SparseVector RHS) {

      // Rprintf("SEMANTIC CLOSURE\n");
      // Rprintf("================\n");
      // Rprintf("A\n");
      // printVector(A);
      // Rprintf("LHS\n");
      // printVector(LHS);

      // Rprintf("RHS\n");
      // printVector(RHS);

      int n_attributes = A.length;

      SparseVector res;
      initVector(&res, n_attributes);

      cloneVector(&res, A);
      // for (int i = 0; i < A.i.used; i++) {
      //
      //   insertArray(&(res.i), A.i.array[i]);
      //   insertArray(&(res.x), A.x.array[i]);
      //
      // }


      if (RHS.p.used == 0) return res;

      int n = RHS.p.used;

      SparseVector res2;
      SparseVector res3;

      initVector(&res2, n_attributes);
      initVector(&res3, n_attributes);


      IntArray subsets;
      initArray(&subsets, n);
      // bool* subsets = (bool*)malloc(n * sizeof(bool));
      bool* black_list = (bool*)malloc(n * sizeof(bool));
      int idx;

      for (int i = 0; i < n; i++) {

        black_list[i] = true;

      }

      is_subset_tree_struct3(A, t, &subsets, black_list);

      // int count = 0;
      while (subsets.used > 0) {

        // count++;
        // Rprintf("%d\n", count);
        // Rprintf("Subsets:\n");
        // printArray(subsets);

        set_union_sparsevector(RHS, subsets, &res2);

        // Rprintf("RES2\n");
        // printVector(res2);

        setunion_struct2(res, res2, &res3);
        // Rprintf("RES3\n");
        // printVector(res3);

        cloneVector(&res, res3);

        // Rprintf("RES\n");
        // printVector(res);

        reinitVector(&res2);
        reinitVector(&res3);

        // Rprintf("RES_again\n");
        // printVector(res);


        for (int i = 0; i < subsets.used; i++) {

          black_list[subsets.array[i]] = false;

        }

        is_subset_tree_struct3(res, t, &subsets, black_list);
        // printArray(subsets);

      }

      freeVector(&res2);
      freeVector(&res3);

      freeArray(&subsets);

      free(black_list);

      return res;

    }

    void semantic_closure_tree4(SparseVector A,
                                ImplicationTree t,
                                SparseVector LHS,
                                SparseVector RHS,
                                SparseVector *res) {

      // Rprintf("SEMANTIC CLOSURE\n");
      // Rprintf("================\n");
      // Rprintf("A\n");
      // printVector(A);
      // Rprintf("LHS\n");
      // printVector(LHS);

      // Rprintf("RHS\n");
      // printVector(RHS);

      int n_attributes = A.length;

      reinitVector(res);

      cloneVector(res, A);
      // for (int i = 0; i < A.i.used; i++) {
      //
      //   insertArray(&(res->i), A.i.array[i]);
      //   insertArray(&(res->x), A.x.array[i]);
      //
      // }


      if (RHS.p.used != 0) {

        int n = RHS.p.used;

        SparseVector res2;
        SparseVector res3;

        initVector(&res2, n_attributes);
        initVector(&res3, n_attributes);

        IntArray subsets;
        initArray(&subsets, n);
        bool* black_list = (bool*)malloc(n * sizeof(bool));
        int idx;

        for (int i = 0; i < n; i++) {

          black_list[i] = true;

        }

        is_subset_tree_struct3(A, t, &subsets, black_list);

        // int count = 0;
        while (subsets.used > 0) {

          // count++;
          // Rprintf("%d\n", count);
          // Rprintf("Subsets:\n");
          // printArray(subsets);

          set_union_sparsevector(RHS, subsets, &res2);

          // Rprintf("RES2\n");
          // printVector(res2);

          setunion_struct2(*res, res2, &res3);
          // Rprintf("RES3\n");
          // printVector(res3);

          cloneVector(res, res3);

          // Rprintf("RES\n");
          // printVector(res);

          reinitVector(&res2);
          reinitVector(&res3);

          // Rprintf("RES_again\n");
          // printVector(res);


          for (int i = 0; i < subsets.used; i++) {

            black_list[subsets.array[i]] = false;

          }

          is_subset_tree_struct3(*res, t, &subsets, black_list);
          // printArray(subsets);

        }

        freeVector(&res2);
        freeVector(&res3);

        freeArray(&subsets);

        free(black_list);

      }

    }



    void semantic_closure_tree2(SparseVector *A,
                                ImplicationTree t,
                                std::list<SparseVector> RHS) {

      int n = RHS.size();

      int n_attributes = A->length;

      SparseVector res, res2;

      initVector(&res, n_attributes);
      initVector(&res2, n_attributes);

      for (int i = 0; i < A->i.used; i++) {

        insertArray(&(res.i), A->i.array[i]);
        insertArray(&(res.x), A->x.array[i]);

      }

      LogicalVector subsets(n);
      LogicalVector black_list(n);
      int idx;

      for (int i = 0; i < n; i++) {

        black_list[i] = true;

      }

      std::list<SparseVector>::iterator it, it2;

      subsets = is_subset_tree_struct(*A, t);

      while (sum(subsets) > 0) {

        idx = 0;

        for (it = RHS.begin(); it != RHS.end(); ++it ) {

          if (subsets[idx]) {

            setunion_struct2(res, (*it), &res2);

            cloneVector(&res, res2);

            reinitVector(&res2);

            black_list[idx] = false;

          }

          idx++;

        }

        subsets = is_subset_tree_struct(res, t);

        subsets = subsets & black_list;

      }

      cloneVector(A, res);
      freeVector(&res);
      freeVector(&res2);

    }

    SparseVector next_closure_implications_struct(SparseVector A, int i,
                                                  int imax,
                                                  List grades_set,
                                                  std::list<SparseVector> LHS,
                                                  std::list<SparseVector> RHS) {

      int n_grades = grades_set.size();
      SparseVector candB;

      initVector(&candB, A.length);

      NumericVector gr;

      for (int a_i = i - 1; i >= 0; a_i--) {

        gr = grades_set[a_i];
        n_grades = gr.size();

        for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

          candB = direct_sum_struct(A, a_i, gr[grade_idx], imax);

          candB = semantic_closure_C(candB, LHS, RHS);

          if (is_set_preceding_struct(A, candB, a_i, gr[grade_idx])) {

            return candB;

          }

        }

      }

      return candB;

    }

    SparseVector next_closure_implications_tree(SparseVector A, int i,
                                                int imax,
                                                ListOf<NumericVector> grades_set,
                                                ImplicationTree t,
                                                std::list<SparseVector> RHS) {

      int n_grades = grades_set.size();
      SparseVector candB;

      for (int a_i = i - 1; i >= 0; a_i--) {

        n_grades = grades_set[a_i].size();

        for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

          candB = direct_sum_struct(A, a_i, grades_set[a_i][grade_idx], imax);

          candB = semantic_closure_tree(candB, t, RHS);

          if (is_set_preceding_struct(A, candB, a_i, grades_set[a_i][grade_idx])) {

            return candB;

          }

        }

      }

      Rprintf("Something went wrong...\n");

      return candB;

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

      // if (A.i.used == 0) {
      //
      //   insertArray(&(candB.i), i - 1);
      //   insertArray(&(candB.x), grades_set[i - 1][0]);
      //
      //   return candB;
      //
      // }

      int n_grades = grades_set.size();
      SparseVector candB2;
      initVector(&candB2, A.length);


      // int firstA = A.i.array[0];

      for (int a_i = i - 1; i >= 0; a_i--) {

        n_grades = grades_set[a_i].size();

        for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

          direct_sum_struct3(A, a_i, grades_set[a_i][grade_idx], imax, &candB);

          //Rprintf("Before SCT3\n");
          //printVector(candB, attrs);
          //Rprintf("\n");

          semantic_closure_tree4(candB, t, LHS, RHS, &candB2);
          cloneVector(&candB, candB2);
          //Rprintf("After SCT3\n");
          //printVector(candB, attrs);
          //Rprintf("\n");

          // if (candB.i.array[0] < firstA) continue;

          if (is_set_preceding_struct(A, candB, a_i, grades_set[a_i][grade_idx])) {

            // Rprintf("Chosen\n");
            freeVector(&candB2);
            return candB;

          }

        }

      }

      Rprintf("Something went wrong...\n");

      return candB;

    }

    SparseVector next_closure_implications_tree2(SparseVector A, int i,
                                                 int imax,
                                                 List grades_set,
                                                 ImplicationTree t,
                                                 std::list<SparseVector> RHS) {

      int n_grades = grades_set.size();

      // initVector(&candB, A.length);

      NumericVector gr;

      for (int a_i = i - 1; i >= 0; a_i--) {

        gr = grades_set[a_i];
        n_grades = gr.size();

        for (int grade_idx = 0; grade_idx < n_grades; grade_idx++) {

          SparseVector candB = direct_sum_struct(A, a_i, gr[grade_idx], imax);

          semantic_closure_tree2(&candB, t, RHS);

          if (is_set_preceding_struct(A, candB, a_i, gr[grade_idx])) {

            return candB;

          }

          freeVector(&candB);

        }

      }

      // return candB;

    }

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




    static void chkIntFn3(void *dummy) {
      R_CheckUserInterrupt();
    }

    // this will call the above in a top-level context so it won't longjmp-out of your context
    bool checkInterrupt3() {
      return (R_ToplevelExec(chkIntFn3, NULL) == FALSE);
    }

    // [[Rcpp::export]]
    List ganters_algorithm_implications_struct(NumericMatrix I,
                                               List grades_set,
                                               int n_attributes) {

      // Timer timer;

      // timer.step("start");

      std::list <S4> res2;
      std::list <SparseVector> LHS;
      std::list <SparseVector> RHS;

      std::list <S4> LHS_S4;
      std::list <S4> RHS_S4;

      SparseVector empty, B, rhs;

      initVector(&empty, n_attributes);
      initVector(&B, n_attributes);
      initVector(&rhs, n_attributes);


      SparseVector A = compute_closure_struct(empty, I);

      if (cardinal_struct(A) > 0) {

        LHS.push_back(empty);
        RHS.push_back(A);

        LHS_S4.push_back(SparseToS4(empty));
        RHS_S4.push_back(SparseToS4(A));

      } else {

        res2.push_back(SparseToS4(A));

      }

      int count = 0;

      while (cardinal_struct(A) < n_attributes) {

        // timer.step("in");
        A = next_closure_implications_struct(A,
                                             n_attributes,
                                             n_attributes,
                                             grades_set,
                                             LHS, RHS);

        // timer.step("next_closure");
        // Rprintf("Antes de closure\n");
        B = compute_closure_struct(A, I);
        // Rprintf("Después de closure\n");

        // timer.step("closure_C");

        rhs = setdifference_struct(B, A);
        // timer.step("setdiff");

        if (cardinal_struct(rhs) == 0) {

          // Concept
          res2.push_back(SparseToS4(A));//[count++] = A;

        } else {

          LHS.push_back(A);
          RHS.push_back(rhs);

          LHS_S4.push_back(SparseToS4(A));
          RHS_S4.push_back(SparseToS4(rhs));

          count++;

          if (count % 10 == 0) Rprintf("%u\n", count);

        }

        // timer.step("push");

        if (checkInterrupt3()) { // user interrupted ...

          List res = List::create(_["concepts"] = wrap(res2),
                                  _["LHS"] = wrap(LHS_S4),
                                  _["RHS"] = wrap(RHS_S4));
          // _["timer"] = timer);

          return res;

        }

      }

      List res = List::create(_["concepts"] = wrap(res2),
                              _["LHS"] = wrap(LHS_S4),
                              _["RHS"] = wrap(RHS_S4));
      // _["timer"] = timer);

      return res;

    }

    // [[Rcpp::export]]
    List ganters_algorithm_implications_tree(NumericMatrix I,
                                             List grades_set,
                                             int n_attributes) {

      // Timer timer;

      // timer.step("start");

      std::list <S4> res2;
      std::list <SparseVector> LHS;
      std::list <SparseVector> RHS;

      std::list <S4> LHS_S4;
      std::list <S4> RHS_S4;

      SparseVector empty, B, rhs;

      initVector(&empty, n_attributes);
      initVector(&B, n_attributes);
      initVector(&rhs, n_attributes);

      // Rprintf("Antes de tree\n");

      ImplicationTree tree;
      initImplicationTree(&tree, n_attributes);

      // Rprintf("Antes de closure_empty\n");
      SparseVector A = compute_closure_struct(empty, I);

      if (cardinal_struct(A) > 0) {

        LHS.push_back(empty);
        RHS.push_back(A);

        LHS_S4.push_back(SparseToS4(empty));
        RHS_S4.push_back(SparseToS4(A));

      } else {

        res2.push_back(SparseToS4(A));

      }

      int count = 0;
      // Rprintf("Antes de while\n");

      double pctg, old_pctg = 0;

      while ((cardinal_struct(A) < n_attributes)){

        A = next_closure_implications_tree(A,
                                           n_attributes,
                                           n_attributes,
                                           grades_set,
                                           tree, RHS);


        // A = next_closure_implications_tree(A,
        //                                      n_attributes,
        //                                      n_attributes,
        //                                      grades_set,
        //                                      tree, RHS);

        pctg = (100 * (n_attributes - A.i.array[0])) / n_attributes;

        if (pctg != old_pctg) {

          Rprintf("Completed = %.2f\n", pctg);
          old_pctg = pctg;

        }
        // Rprintf("NextClosure:\n");
        // printArray(A.i);

        // Rprintf("Antes de closure\n");
        B = compute_closure_struct(A, I);

        // Rprintf("Closure:\n");
        // printArray(B.i);


        rhs = setdifference_struct(B, A);

        if (cardinal_struct(rhs) == 0) {

          // Concept
          res2.push_back(SparseToS4(A));//[count++] = A;

        } else {

          // LHS.push_back(A);
          RHS.push_back(rhs);

          // Rprintf("Added implication with RHS:\n");
          // printArray(rhs.i);

          LHS_S4.push_back(SparseToS4(A));
          RHS_S4.push_back(SparseToS4(rhs));

          addImplicationToTree(&tree, A);

          count++;

          if (count % 10 == 0) Rprintf("%u\n", count);

        }

        // timer.step("push");

        if (checkInterrupt3()) { // user interrupted ...

          List res = List::create(_["concepts"] = wrap(res2),
                                  _["LHS"] = wrap(LHS_S4),
                                  _["RHS"] = wrap(RHS_S4));
          // _["timer"] = timer);

          return res;

        }

      }

      List res = List::create(_["concepts"] = wrap(res2),
                              _["LHS"] = wrap(LHS_S4),
                              _["RHS"] = wrap(RHS_S4));
      // _["timer"] = timer);

      return res;

    }

    bool checkValidity(SparseVector A) {

      for (int i = 0; i < A.i.used; i++) {

        if ((A.x.array[i] < 0.1) & (A.x.array[i] > 0)) return false;

      }

      return true;

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

          if (count % 10 == 0) Rprintf("%u\n", count);

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

      Rprintf("Finished.\n");

      return res;

    }

    // [[Rcpp::export]]
    LogicalVector is_subset_struct(List L, S4 S) {

      int n_implications = L.size();

      std::list<SparseVector> LHS;
      SparseVector Ssp = S4toSparse(S);

      for (int i = 0; i < n_implications; i++) {

        LHS.push_back(S4toSparse(L[i]));

      }

      LogicalVector subsets(n_implications);
      int idx;


      std::list<SparseVector>::iterator it, it2;

      idx = 0;
      for (it2 = LHS.begin(); it2 != LHS.end(); ++it2 ) {

        subsets[idx++] = is_subset_one_struct((*it2), Ssp);

      }

      return(subsets);

    }

    // [[Rcpp::export]]
    void init_is_subset_struct(List L, S4 S) {

      int n_implications = L.size();

      std::vector<SparseVector> LHS;
      SparseVector Ssp = S4toSparse(S);

      for (int i = 0; i < n_implications; i++) {

        LHS.push_back(S4toSparse(L[i]));
      }

    }

    // [[Rcpp::export]]
    S4 compute_closure_struct_C(S4 A, NumericMatrix I,
                                StringVector attributes) {

      SparseVector Ssp = S4toSparse(A);

      SparseVector B = compute_closure_struct(Ssp, I);

      printVector(B, attributes);

      return SparseToS4(B);


    }
