# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

print_matrix <- function(I) {
    invisible(.Call(`_fcaR_print_matrix`, I))
}

print_vector <- function(I, sz) {
    invisible(.Call(`_fcaR_print_vector`, I, sz))
}

get_element_array <- function(I, i, j, k) {
    .Call(`_fcaR_get_element_array`, I, i, j, k)
}

next_closure_implications <- function(I, grades_set, attrs, save_concepts = TRUE, verbose = FALSE) {
    .Call(`_fcaR_next_closure_implications`, I, grades_set, attrs, save_concepts, verbose)
}

next_closure_implications_bg <- function(I, grades_set, attrs, lhs_bg, rhs_bg, n_bg, save_concepts = TRUE, verbose = FALSE) {
    .Call(`_fcaR_next_closure_implications_bg`, I, grades_set, attrs, lhs_bg, rhs_bg, n_bg, save_concepts, verbose)
}

next_closure_implications_bg2 <- function(I, grades_set, attrs, lhs_bg, rhs_bg, n_bg, save_concepts = TRUE, verbose = FALSE) {
    .Call(`_fcaR_next_closure_implications_bg2`, I, grades_set, attrs, lhs_bg, rhs_bg, n_bg, save_concepts, verbose)
}

next_closure_concepts <- function(I, grades_set, attrs, verbose = FALSE, ret = TRUE) {
    .Call(`_fcaR_next_closure_concepts`, I, grades_set, attrs, verbose, ret)
}

compute_intent <- function(V, I) {
    .Call(`_fcaR_compute_intent`, V, I)
}

compute_intentSpM <- function(V, I) {
    .Call(`_fcaR_compute_intentSpM`, V, I)
}

compute_extent <- function(V, I) {
    .Call(`_fcaR_compute_extent`, V, I)
}

compute_extentSpM <- function(V, I) {
    .Call(`_fcaR_compute_extentSpM`, V, I)
}

compute_closure <- function(V, I) {
    .Call(`_fcaR_compute_closure`, V, I)
}

compute_closureSpM <- function(V, I) {
    .Call(`_fcaR_compute_closureSpM`, V, I)
}

self_intersection_C <- function(x_i, x_p, y_i, y_p) {
    .Call(`_fcaR_self_intersection_C`, x_i, x_p, y_i, y_p)
}

is_subset_C <- function(X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P) {
    .Call(`_fcaR_is_subset_C`, X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P)
}

intersects_C <- function(X_P, X_I, X_DIM, Y_P, Y_I, Y_DIM, OUT_P) {
    .Call(`_fcaR_intersects_C`, X_P, X_I, X_DIM, Y_P, Y_I, Y_DIM, OUT_P)
}

is_equal_set_C <- function(X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P) {
    .Call(`_fcaR_is_equal_set_C`, X_P, X_I, X_DIM, X, Y_P, Y_I, Y_DIM, Y, PROPER, OUT_P)
}

which_at_col <- function(x_i, x_p, col) {
    .Call(`_fcaR_which_at_col`, x_i, x_p, col)
}

set_difference <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_difference`, xi, xp, xx, yi, yp, yx, number)
}

set_difference_SpM <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_difference_SpM`, xi, xp, xx, yi, yp, yx, number)
}

set_difference_single <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_difference_single`, xi, xp, xx, yi, yp, yx, number)
}

set_difference_single_SpM <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_difference_single_SpM`, xi, xp, xx, yi, yp, yx, number)
}

set_union_sparse <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_union_sparse`, xi, xp, xx, yi, yp, yx, number)
}

set_union_SpM <- function(xi, xp, xx, yi, yp, yx, number) {
    .Call(`_fcaR_set_union_SpM`, xi, xp, xx, yi, yp, yx, number)
}

flatten_sparse_C <- function(p, i, x, dims) {
    .Call(`_fcaR_flatten_sparse_C`, p, i, x, dims)
}

flatten_sparse_SpM <- function(p, i, x, dims) {
    .Call(`_fcaR_flatten_sparse_SpM`, p, i, x, dims)
}

transposeSpM <- function(A) {
    .Call(`_fcaR_transposeSpM`, A)
}

