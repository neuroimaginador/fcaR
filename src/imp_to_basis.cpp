#include "vector_operations.h"
#include <Rcpp.h>
#include <algorithm>
#include <deque>
#include <vector>

using namespace Rcpp;

// Wrapper for SparseVector to handle memory automatically (basic RAII)
class SparseVecWrapper {
public:
  SparseVector v;
  bool needs_free;

  SparseVecWrapper() {
    v.i.array = NULL;
    v.i.used = 0;
    v.i.size = 0;
    v.p.array = NULL;
    v.p.used = 0;
    v.p.size = 0;
    v.x.array = NULL;
    v.x.used = 0;
    v.x.size = 0;
    v.length = 0;
    needs_free = false;
  }

  // Take ownership of a raw SparseVector
  SparseVecWrapper(SparseVector sv) : v(sv), needs_free(true) {}

  // Copy constructor (deep copy)
  SparseVecWrapper(const SparseVecWrapper &other) {
    // Initialize with dimension
    size_t dim = other.v.length;
    if (dim == 0 && other.v.i.size > 0)
      dim = other.v.i.size; // fallback or 1
    if (dim == 0)
      dim = 1;

    initVector(&v, dim);

    const SparseVector &src = other.v;
    for (size_t k = 0; k < src.i.used; ++k) {
      insertArray(&v.i, src.i.array[k]);
      insertArray(&v.x, src.x.array[k]);
    }
    insertArray(&v.p, 0);
    if (src.p.used > 0)
      insertArray(&v.p, src.p.array[src.p.used - 1]);
    else
      insertArray(&v.p, src.i.used); // usually p is simple for vector.

    needs_free = true;
  }

  // Destructor
  ~SparseVecWrapper() {
    if (needs_free) {
      freeVector(&v);
    }
  }

  // Assignment (deep copy)
  SparseVecWrapper &operator=(const SparseVecWrapper &other) {
    if (this != &other) {
      if (needs_free)
        freeVector(&v);

      // Same logic as copy constructor
      size_t dim = other.v.length;
      if (dim == 0)
        dim = 10;
      initVector(&v, dim);

      const SparseVector &src = other.v;
      for (size_t k = 0; k < src.i.used; ++k) {
        insertArray(&v.i, src.i.array[k]);
        insertArray(&v.x, src.x.array[k]);
      }
      // Restore p structure
      freeArray(&v.p); // initVector created empty p array
      initArray(&v.p, 2);
      insertArray(&v.p, 0);
      insertArray(&v.p, v.i.used);

      needs_free = true;
    }
    return *this;
  }

  SparseVector *get() { return &v; }
  const SparseVector &ref() const { return v; }
};

// Implication structure
struct Implication {
  SparseVecWrapper lhs;
  SparseVecWrapper rhs;
};

// Helper: Check if A is subset of B (Sparse)
// Assumes indices are sorted (standard format)
bool is_subset_sparse(const SparseVector &A, const SparseVector &B) {
  size_t i = 0, j = 0;
  while (i < A.i.used && j < B.i.used) {
    if (A.i.array[i] < B.i.array[j])
      return false; // element in A not in B
    if (A.i.array[i] > B.i.array[j]) {
      j++;
      continue;
    }
    // Indices match
    if (A.x.array[i] > B.x.array[j])
      return false; // value in A > value in B
    i++;
    j++;
  }
  return i == A.i.used;
}

// Helper: Union (Return A U B)
SparseVecWrapper union_sparse(const SparseVecWrapper &A_wrapper,
                              const SparseVecWrapper &B_wrapper) {
  SparseVecWrapper res_wrapper;
  // Initialize res with A's length
  size_t dim = A_wrapper.v.length;
  if (dim == 0)
    dim = 1;
  initVector(&res_wrapper.v, dim);

  const SparseVector &A = A_wrapper.v;
  const SparseVector &B = B_wrapper.v;

  SparseVector *res = &res_wrapper.v;

  size_t i = 0, j = 0;
  while (i < A.i.used && j < B.i.used) {
    if (A.i.array[i] < B.i.array[j]) {
      insertArray(&res->i, A.i.array[i]);
      insertArray(&res->x, A.x.array[i]);
      i++;
    } else if (A.i.array[i] > B.i.array[j]) {
      insertArray(&res->i, B.i.array[j]);
      insertArray(&res->x, B.x.array[j]);
      j++;
    } else {
      insertArray(&res->i, A.i.array[i]);
      insertArray(&res->x, std::max(A.x.array[i], B.x.array[j]));
      i++;
      j++;
    }
  }
  while (i < A.i.used) {
    insertArray(&res->i, A.i.array[i]);
    insertArray(&res->x, A.x.array[i]);
    i++;
  }
  while (j < B.i.used) {
    insertArray(&res->i, B.i.array[j]);
    insertArray(&res->x, B.x.array[j]);
    j++;
  }

  insertArray(&res->p, 0);
  insertArray(&res->p, res->i.used);

  res_wrapper.needs_free = true;
  return res_wrapper;
}

// Helper: Compute Closure of set S under implications in 'queue'
// queue contains the VALID implications to use.
SparseVecWrapper
compute_closure_from_queue(const SparseVector &S_in,
                           const std::deque<Implication> &queue) {

  // Make a DEEP COPY of S_in to work on.
  // We cannot use SparseVecWrapper(S_in) because S_in is a SparseVector (not
  // Wrapper), so it would call the "Take Ownership" constructor (shallow copy
  // of pointers), leading to Double Free (S shares memory with the source).

  SparseVecWrapper S;
  size_t dim = S_in.length;
  if (dim == 0)
    dim = 1;
  initVector(&S.v, dim);

  // Manual deep copy
  for (size_t k = 0; k < S_in.i.used; ++k) {
    insertArray(&S.v.i, S_in.i.array[k]);
    insertArray(&S.v.x, S_in.x.array[k]);
  }
  insertArray(&S.v.p, 0);
  if (S_in.p.used > 0)
    insertArray(&S.v.p, S_in.p.array[S_in.p.used - 1]);
  else
    insertArray(&S.v.p, S_in.i.used);

  S.needs_free = true;

  // In strict LinClosure, we iterate until stability.
  // We can optimize by only checking applicable rules.

  bool changed = true;
  // We can keep a "used" mask for THIS closure computation
  // But since S grows, a rule might become applicable later.
  // However, once a rule is applied, its RHS is added to S.
  // Re-applying it won't change S (idempotency of Union).
  // So we can mark rules as "applied".

  std::vector<bool> applied(queue.size(), false);

  while (changed) {
    changed = false;

    for (size_t k = 0; k < queue.size(); ++k) {
      if (applied[k])
        continue;

      if (is_subset_sparse(queue[k].lhs.ref(), S.ref())) {
        // Determine if we need to apply
        // Optimization: check if RHS is already subset of S
        if (is_subset_sparse(queue[k].rhs.ref(), S.ref())) {
          applied[k] = true;
          continue; // No change needed
        }

        // Apply rule
        // S = S U RHS
        S = union_sparse(S, queue[k].rhs);
        applied[k] = true;
        changed = true;

        // If changed, we continue loop to check other rules
      }
    }
  }
  return S;
}

// Helper: Difference2 (A \ B) - similar to R's .difference2 or setdifference
// In R code: .difference2(RHS, LHS) at the very end.
// And inside loop: if (!(all(A == B))) check.
// We need an equality check or just use is_subset both ways.

// [[Rcpp::export]]
List imp_to_basis_cpp(S4 lhs, S4 rhs) {
  // Rcout << "Starting imp_to_basis_cpp" << std::endl;

  // Parse LHS
  IntegerVector lhs_i = lhs.slot("i");
  IntegerVector lhs_p = lhs.slot("p");
  NumericVector lhs_x = lhs.slot("x");
  int n_attrs = 0;
  // Wait, in fcaR implications: attributes are ROWS (if distinct), implications
  // are COLS? Usually implications: (n_attributes x n_implications). Let's
  // assume standard Sparse Matrix structure.
  IntegerVector dim = lhs.slot("Dim");
  int n_rows = dim[0];
  int n_cols = dim[1];
  n_attrs = n_rows;
  int n_imps = n_cols;

  // Parse RHS
  IntegerVector rhs_i = rhs.slot("i");
  IntegerVector rhs_p = rhs.slot("p");
  NumericVector rhs_x = rhs.slot("x");

  // Rcout << "n_imps: " << n_imps << " n_attrs: " << n_attrs << std::endl;

  std::deque<Implication> queue;

  for (int k = 0; k < n_imps; ++k) {
    // LHS column k
    SparseVector l;
    initVector(&l, n_attrs);

    int p_start = lhs_p[k];
    int p_end = lhs_p[k + 1];

    for (int idx = p_start; idx < p_end; ++idx) {
      insertArray(&l.i, lhs_i[idx]);
      insertArray(&l.x, lhs_x[idx]);
    }
    insertArray(&l.p, 0);
    insertArray(&l.p, l.i.used);

    // RHS column k
    SparseVector r;
    initVector(&r, n_attrs);

    p_start = rhs_p[k];
    p_end = rhs_p[k + 1];

    for (int idx = p_start; idx < p_end; ++idx) {
      insertArray(&r.i, rhs_i[idx]);
      insertArray(&r.x, rhs_x[idx]);
    }
    insertArray(&r.p, 0);
    insertArray(&r.p, r.i.used);

    queue.push_back(Implication{SparseVecWrapper(l), SparseVecWrapper(r)});

    // SparseVecWrapper constructor (taking SparseVector by value) takes
    // ownership of the arrays. The temporary wrapper created for Implication
    // constructor will free them when it dies. So we must NOT free them
    // manually here.
  }

  // No need to free LHS_sv/RHS_sv as we didn't create them.

  // 2. Main Loop
  // Matches R logic: for (i in seq(n))
  int original_n = n_imps;

  for (int i = 0; i < original_n; ++i) {
    // Rcout << "Processing Main Loop i=" << i << std::endl;
    // Pop the first implication (simulating LHS[, 1])
    Implication current = queue.front();
    queue.pop_front();
    // Rcout << "Popped current" << std::endl;

    // A = current.lhs, B = current.rhs
    // AUB = A U B

    // Calculate AUB
    SparseVecWrapper AUB = union_sparse(current.lhs, current.rhs);

    // Compute Closure of AUB using remaining rules in queue
    // In R: .compute_closure(AUB, LHS, RHS...) where LHS/RHS are the REST.
    // Here 'queue' acts as the REST.
    SparseVecWrapper NewB = compute_closure_from_queue(AUB.ref(), queue);

    // Update LHS, RHS
    // LHS <- cbind(LHS, A)  => Push A to back
    // RHS <- cbind(RHS, B)  => Push NewB to back

    // We create new implication with original A and NEW B
    SparseVecWrapper new_lhs(current.lhs); // Copy original A
    // NewB is already a valid wrapper

    queue.push_back(Implication{new_lhs, NewB});
    // Rcout << "Pushed new implication" << std::endl;

    // 'current' goes out of scope, destructor frees old A and B data
  }

  // 3. Second Pass (if needed)
  // R code has TWO loops.
  // Loop 1: Updates closures.
  // Loop 2: Checks redundancy?
  /*
    for (i in seq(n)) {
      A <- LHS[, 1]; B <- RHS[, 1]
      LHS <- LHS[, -1]; RHS <- RHS[, -1]

      A_closure <- .compute_closure(A, LHS, RHS...)

      if (!(all(A_closure == B))) {
         LHS <- cbind(LHS, A_closure) ??? No wait.

         R code:
         A <- .compute_closure(A, ...)
         if (!all(A == B)) {
            LHS <- cbind(LHS, A)
            RHS <- cbind(RHS, B)
         }
      }
    }

    Wait, in R loop 2:
    It computes closure of A (LHS).
    If closure of A is NOT equal to B (RHS), it keeps it.
    If closure of A == B, it implies A -> B is redundant or covered?
    Wait, if A == B, then A -> B is A -> A, which is trivial.

    Wait, look at R code loop 2 carefully.
    A <- LHS[, 1]
    B <- RHS[, 1]
    LHS/RHS removed.

    A_new <- compute_closure(A, remaining...)

    if (!(all(A_new == B))) {
       LHS <- cbind(LHS, A)  <-- Uses ORIGINAL A? No, 'A' variable was
    overwritten by A_new! RHS <- cbind(RHS, B)
    }

    Wait:
    Line 33: A <- .compute_closure(A, ...)
    So 'A' becomes the closure of the premise.

    If A (closure) != B (the rhs stored), then we keep it?

    Actually, if A -> B is a valid implication in the basis:
    A's closure should be B?

    Let's trace:
    In Loop 1, we replaced B with Closure(A U B).
    So B is now closed.

    In Loop 2:
    We take A. We compute Closure(A) using OTHER rules.
    If Closure(A) == B, then A -> B is implied by others?
    If Closure(A) == B, then we DO NOT add it back.
    So we remove trivial/redundant implications.

    Re-reading R Code Loop 2:
    Line 36: if (!(all(A == B))) { ... cbind ... }

    So if Closure of A is DIFFERENT from B, we keep (A, B).
    If Closure of A IS EQUAL to B, we discard.

    Wait, B is the closure calculated in Loop 1 (A U B closed).
    If Closure(A) with others is already B, then this rule adds nothing?
    Yes, that's the logic.
  */

  int n_after_pass1 = queue.size();
  std::deque<Implication> final_queue;

  for (int i = 0; i < n_after_pass1; ++i) {
    Implication current = queue.front();
    queue.pop_front(); // Remove from potential set

    // Check closure of A using the REST (which is queue + final_queue?)
    // In R:
    // LHS <- LHS[, -1] (removes current)
    // A <- .compute_closure(..., remaining LHS/RHS)
    // Note: In R, 'remaining' includes those processed in this loop (appended)
    // and those waiting? In R loop 2: LHS/RHS are shrinking from front and
    // growing at back if condition met. So the context for closure includes
    // relevant rules.

    // In my C++:
    // I have 'queue' (remaining original set) and 'final_queue' (kept ones).
    // I must use BOTH for closure?
    // R logic:
    // LHS variable is modified in place.
    // When calling .compute_closure(..., LHS, RHS), it uses the CURRENT state
    // of LHS/RHS. Which contains:
    // 1. Examples waiting to be processed (indices i+1 to n)
    // 2. Examples already processed and KEPT (indices appended)

    // So I need a combined set of rules.
    // I can construct a temporary deque combining final_queue and queue?
    // Or write compute_closure to accept two ranges?
    // Or just move elements around.

    // Let's create a combined deque for closure calculation.
    std::deque<Implication> combined_rules;
    combined_rules.insert(combined_rules.end(), queue.begin(), queue.end());
    combined_rules.insert(combined_rules.end(), final_queue.begin(),
                          final_queue.end());

    SparseVecWrapper A_closure =
        compute_closure_from_queue(current.lhs.ref(), combined_rules);

    // Equality Check: A_closure == current.rhs
    // We know A_closure >= current.lhs (property of closure)
    // current.rhs is also a closure of (A U B_orig).
    // If A -> B is redundant, then Closure(A) (without this rule) should
    // contain B. Checking identity A_closure == current.rhs might be strict.
    // But R does `all(A == B)`.

    bool equal = true;
    if (A_closure.ref().i.used != current.rhs.ref().i.used)
      equal = false;
    else {
      for (size_t k = 0; k < A_closure.ref().i.used; ++k) {
        if (A_closure.ref().i.array[k] != current.rhs.ref().i.array[k] ||
            std::abs(A_closure.ref().x.array[k] -
                     current.rhs.ref().x.array[k]) > 1e-9) {
          equal = false;
          break;
        }
      }
    }

    if (!equal) {
      // Keep it
      // Note: In R Loop 2, it does:
      // LHS <- cbind(LHS, A)  <-- Wait, it uses 'A' which is the CLOSURE?
      // Line 38: LHS <- cbind(LHS, A)
      // But A was overwritten by line 33: A <- .compute_closure(...)
      // So LHS becomes the CLOSURE of A.
      // And RHS becomes B.

      // So the rule becomes Closure(A) -> B ?
      // Let's verify R code again.
      // 33: A <- .compute_closure(...)
      // 38: LHS <- cbind(LHS, A)
      // Yes. It replaces LHS with its closure.

      // So we add (A_closure, current.rhs) to final_queue.
      final_queue.push_back(Implication{
          A_closure, current.rhs}); // A_closure copy, current.rhs copy

    } else {
      // Drop it.
    }
  }

  // 4. Final Result Construction
  // Return list(lhs = ..., rhs = .difference2(RHS, LHS))
  // Note .difference2(RHS, LHS) -> B \ A

  // Convert final_queue to S4
  int final_n = final_queue.size();

  // We need to re-assemble CSC matrices.
  // Count total non-zeros
  int total_nz_lhs = 0;
  int total_nz_rhs = 0; // This will start as RHS

  // For RHS output, we need B \ A
  // In R: .difference2(RHS, LHS)
  // Here: for each imp in final_queue, result_rhs = imp.rhs \ imp.lhs

  // Let's compute diffs and store temporarily
  std::vector<SparseVecWrapper> final_diffs;
  final_diffs.reserve(final_n);

  for (int k = 0; k < final_n; ++k) {
    SparseVecWrapper U = final_queue[k].lhs; // This is the LHS to return
    SparseVecWrapper V = final_queue[k].rhs;

    // Compute Diff V \ U
    SparseVector Diff;
    // logic: if U subset V?
    // .difference2 implementation (Step 25):
    // A[B >= A] <- 0  (where A is LHS arg, B is RHS arg? No, diff(A, B) -> A
    // \ B) .difference2(A, B) -> A \ B. R Call: .difference2(RHS, LHS). So RHS
    // \ LHS.

    // My helpers set_difference(x, y) -> x \ y
    // Using setdifference from set_operations_galois.cpp logic
    // setdifference(SparseVector x, SparseVector y)

    // I'll implement a quick set_difference_sparse
    // Because I can't link easily to valid .o files without attributes mess?
    // Actually I can define it here.

    // Diff = V \ U
    initVector(&Diff, n_attrs);
    // Logic: keep elements of V that are NOT in U or > U

    size_t i = 0, j = 0; // i for V, j for U
    const SparseVector &v_vec = V.ref();
    const SparseVector &u_vec = U.ref();

    while (i < v_vec.i.used) {
      bool keep = true;
      // Check against U
      // Find matching index in U
      while (j < u_vec.i.used && u_vec.i.array[j] < v_vec.i.array[i])
        j++;

      if (j < u_vec.i.used && u_vec.i.array[j] == v_vec.i.array[i]) {
        if (u_vec.x.array[j] >= v_vec.x.array[i]) {
          keep = false; // Completely covered
        }
        // If u < v, we keep v (or difference?)
        // In fuzzy sets, difference might be max(0, v - u) or just v if v > u?
        // R .difference2: A[B >= A] <- 0.  (A is first arg).
        // It zeroes out if covered. Else keeps A?
        // R code: A[B >= A] <- 0.
        // It doesn't subtract. It just keeps original value or 0.

        // So if u >= v, we drop.
        // If u < v, we keep v.
      }

      if (keep) {
        insertArray(&Diff.i, v_vec.i.array[i]);
        insertArray(&Diff.x, v_vec.x.array[i]);
      }
      i++;
    }
    insertArray(&Diff.p, 0);
    insertArray(&Diff.p, Diff.i.used);

    final_diffs.push_back(SparseVecWrapper(Diff));

    total_nz_lhs += U.ref().i.used;
    total_nz_rhs += Diff.i.used;
  }

  // Construct LHS S4
  std::vector<int> lhs_i_vec, lhs_p_vec;
  std::vector<double> lhs_x_vec;
  lhs_p_vec.push_back(0);
  lhs_i_vec.reserve(total_nz_lhs);
  lhs_x_vec.reserve(total_nz_lhs);

  for (int k = 0; k < final_n; ++k) {
    const SparseVector &v = final_queue[k].lhs.ref();
    for (size_t idx = 0; idx < v.i.used; ++idx) {
      lhs_i_vec.push_back(v.i.array[idx]);
      lhs_x_vec.push_back(v.x.array[idx]);
    }
    lhs_p_vec.push_back(lhs_p_vec.back() + v.i.used);
  }

  S4 out_lhs("dgCMatrix");
  out_lhs.slot("i") = wrap(lhs_i_vec);
  out_lhs.slot("p") = wrap(lhs_p_vec);
  out_lhs.slot("x") = wrap(lhs_x_vec);
  out_lhs.slot("Dim") = IntegerVector::create(n_attrs, final_n);

  // Construct RHS S4
  std::vector<int> rhs_i_vec, rhs_p_vec;
  std::vector<double> rhs_x_vec;
  rhs_p_vec.push_back(0);
  rhs_i_vec.reserve(total_nz_rhs);
  rhs_x_vec.reserve(total_nz_rhs);

  for (int k = 0; k < final_n; ++k) {
    const SparseVector &v = final_diffs[k].ref();
    for (size_t idx = 0; idx < v.i.used; ++idx) {
      rhs_i_vec.push_back(v.i.array[idx]);
      rhs_x_vec.push_back(v.x.array[idx]);
    }
    rhs_p_vec.push_back(rhs_p_vec.back() + v.i.used);
  }

  S4 out_rhs("dgCMatrix");
  out_rhs.slot("i") = wrap(rhs_i_vec);
  out_rhs.slot("p") = wrap(rhs_p_vec);
  out_rhs.slot("x") = wrap(rhs_x_vec);
  out_rhs.slot("Dim") = IntegerVector::create(n_attrs, final_n);

  return List::create(_["lhs"] = out_lhs, _["rhs"] = out_rhs);
}
