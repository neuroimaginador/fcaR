# SpM_struct
#' @importFrom methods slotNames
#' @importFrom rlang env_clone
new_spm <- function(i, p, x, nrow,
                    dimnames = list(NULL, NULL)) {

  private <- list()

  if (inherits(i, "Matrix")) {

    dimnames <- dimnames(i)

    if ("x" %in% methods::slotNames(i)) {

      x <- i@x

    } else {

      x <- rep(1, length(i@i))

    }
    p <- i@p
    nrow <- dim(i)[1]
    i <- i@i + 1

  }

  if (inherits(i, "SpM")) {

    return(rlang::env_clone(i))

  }

  if (inherits(i, "matrix")) {

    dimnames <- dimnames(i)

    nrow <- dim(i)[1]
    idx <- which(i > 0)
    x <- i[idx]
    cols <- seq(ncol(i))
    i <- lapply(cols,
                function(j) which(i[, j] > 0))
    p <- c(0, cumsum(sapply(i, length)))

  }

  if (!is.list(i) & is.null(p)) {

    stop("If i is a vector, you must supply p.",
         call. = FALSE)

  }

  if (is.list(i)) {

    private$pi <- as.integer(unlist(i))
    private$px <- unlist(x)
    private$pp <- as.integer(c(0, cumsum(sapply(i, length))))

  } else {

    if (missing(x)) x <- rep(1, length(i))

    private$pi <- as.integer(i)
    private$px <- x
    private$pp <- as.integer(p)

  }

  # private$validate()

  private$pnrow <- as.integer(nrow)
  private$dimnames <- dimnames
  private$dim <- c(nrow, length(private$pp) - 1)

  private <- as.environment(private)

  structure(private,
            nrow = nrow,
            ncol = length(private$pp) - 1,
            class = "SpM")

}

listing2 <- function(M) {

  i <- M$pi
  p <- M$pp
  x <- M$px

  ncolumn <- length(p)
  if (p[1] == 0) p <- p + 1
  p_init <- p[-ncolumn]
  p_end <- p[-1]

  if (length(p_init) > 1) {

    idx <- mapply(`:`, p_init, p_end, SIMPLIFY = FALSE) %>%
      lapply(function(ii) ii[-length(ii)])

  } else {

    idx <- p_init:p_end
    idx <- idx[-length(idx)]
    idx <- list(idx)

  }

  i_list <- idx %>% lapply(function(id) i[id])
  x_list <- idx %>% lapply(function(id) x[id])

  return(list(pi_list = i_list,
              px_list = x_list))

}

#' @importFrom rlang env_has
to_matrix.SpM <- function(private) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  # private <- append(private, listing2(private))

  i <- private$pi_list
  p <- private$pp
  x <- private$px_list
  n <- private$pnrow
  m <- length(private$pp) - 1

  j <- seq_along(p[-length(p)])
  idx <- j %>%
    sapply(function(jj) i[[jj]] + (jj - 1) * n) %>%
    unlist() %>% as.vector()

  A <- matrix(data = 0,
              nrow = n, ncol = m)
  A[idx] <- unlist(x)
  dimnames(A) <- private$dimnames

  return(A)

}

#' @importFrom rlang env_has
to_logical.SpM <- function(private) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  i <- private$pi_list
  p <- private$pp
  x <- private$px_list
  n <- private$pnrow
  m <- length(private$pp) - 1

  j <- seq_along(p[-length(p)])
  idx <- j %>%
    sapply(function(jj) i[[jj]] + (jj - 1) * n) %>%
    unlist()

  A <- matrix(data = FALSE,
              nrow = n, ncol = m)
  A[idx] <- TRUE
  dimnames(A) <- private$dimnames

  return(A)

}

extract_columns <- function(private, idx) {

  dp <- diff(private$pp)
  dp <- dp[idx]

  dn <- private$dimnames
  if (length(dn[[2]]) > 0)
    dn[[2]] <- dn[[2]][idx]

  p_init <- private$pp[idx]
  p_end <- private$pp[idx + 1]
  if (length(p_init) > 1) {

    idx <- mapply(`:`, p_init, p_end, SIMPLIFY = FALSE) %>%
      lapply(function(ii) ii[-length(ii)]) %>%
      unlist()

  } else {

    idx <- p_init:p_end
    idx <- idx[-length(idx)]

  }

  new_i <- private$pi[idx + 1]
  new_x <- private$px[idx + 1]
  new_p <- c(0, cumsum(dp))

  return(new_spm(i = new_i,
                 x = new_x,
                 p = new_p,
                 nrow = private$pnrow,
                 dimnames = dn))

}

# @export
# @noRd
# `[.SpM` <- function(x, ...) x %>% extract_columns(...)
#'
remove_columns = function(private, idx) {

  dn <- private$dimnames
  if (length(dn[[2]]) > 0)
    dn[[2]] <- dn[[2]][-idx]

  p_init <- private$pp[idx]
  p_end <- private$pp[idx + 1]
  dp <- diff(private$pp)
  dp <- dp[-idx]

  if (length(p_init) > 1) {

    idx <- mapply(`:`, p_init, p_end, SIMPLIFY = FALSE) %>%
      lapply(function(ii) ii[-length(ii)]) %>%
      unlist()

  } else {

    idx <- p_init:p_end
    idx <- idx[-length(idx)]

  }

  if (length(idx) > 0) {

    new_i <- private$pi[-(idx + 1)]
    new_x <- private$px[-(idx + 1)]

  } else {

    new_i <- private$pi
    new_x <- private$px

  }
  new_p <- c(0, cumsum(dp))
  return(new_spm(i = new_i,
                 x = new_x,
                 p = new_p,
                 nrow = private$pnrow,
                 dimnames = dn))

}

#' @importFrom rlang env_has
insert_columns = function(private,
                          M,
                          after = length(private$pp) - 1) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  if (!rlang::env_has(env = M, "pi_list")) {

    L <- listing2(M)
    assign("pi_list", L$pi_list, envir = M)
    assign("px_list", L$px_list, envir = M)

  }

  private$pi_list <- append(private$pi_list, M$pi_list, after = after)
  private$px_list <- append(private$px_list, M$px_list, after = after)

  private$pi <- unlist(private$pi_list)
  private$px <- unlist(private$px_list)
  private$pp <- c(0, cumsum(sapply(private$pi_list, length)))

  dn <- private$dimnames
  # if (length(dn) > 1)
  dn[[2]] <- append(dn[[2]], M$dimnames[[2]], after = after)
  private$dimnames <- dn

  attr(private, "ncol") <- attr(private, "ncol") + ncol.SpM(M)

}

#' @importFrom rlang env_has
substitute_columns = function(private, idx, M) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  if (!rlang::env_has(env = M, "pi_list")) {

    L <- listing2(M)
    assign("pi_list", L$pi_list, envir = M)
    assign("px_list", L$px_list, envir = M)

  }

  private$pi_list[idx] <- M$pi_list
  private$px_list[idx] <- M$px_list

  private$pi <- unlist(private$pi_list)
  private$px <- unlist(private$px_list)
  private$pp <- c(0, cumsum(sapply(private$pi_list, length)))

}

nrow.SpM <- function(private) {

  as.integer(private$pnrow)

}

# nrow <- nrow.SpM

ncol.SpM <- function(private) {

  as.integer(length(private$pp) - 1)

}

# ncol <- ncol.SpM

dim.SpM <- function(private) {

  c(nrow.SpM(private), ncol.SpM(private))

}

# dim <- dim.SpM

#' @importFrom rlang env_has
colSums <- function(private) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  dn <- private$dimnames
  if (!is.null(dn[[2]])) {

    dn <- dn[[2]]

  } else {

    dn <- FALSE

  }

  sapply(private$px_list, sum)

}

replicate <- function(private, n) {

  new_i <- rep(private$pi, times = n)
  new_x <- rep(private$px, times = n)
  new_p <- c(0, cumsum(rep(diff(private$pp), n)))
  dn <- private$dimnames
  # if (length(dn) > 1)
  dn[[2]] <- rep(dn[[2]], times = n)

  return(new_spm(i = new_i,
                 x = new_x,
                 p = new_p,
                 nrow = private$pnrow,
                 dimnames = dn))

}

#' @export
print.SpM <- function(x, ...) {

  matp <- .print_matrix(M = to_matrix.SpM(x),
                objects = x$dimnames[[1]],
                attributes = x$dimnames[[2]])

  M <- matp$mat
  cat(M)

}

.S3methods("print", "SpM")

zeroSpM <- function(nrow, ncol) {

  new_spm(i = integer(0),
          p = c(0, rep(0, ncol)),
          x = numeric(0),
          nrow = nrow)

}

unionSpM <- function(A, B) {

  applicable <- (ncol.SpM(B) == ncol.SpM(A)) ||
    (ncol.SpM(B) == 1) || (ncol.SpM(A) == 1)
  stopifnot(applicable)

  if (ncol.SpM(A) == ncol.SpM(B)) {

    L <- set_union_SpM(A$pi - 1,
                       A$pp,
                       A$px,
                       B$pi - 1,
                       B$pp,
                       B$px,
                       nrow.SpM(A))
    L$dimnames <- A$dimnames

    return(L)

  }

  if (ncol.SpM(B) == 1) {

    n <- ncol.SpM(A)
    return(unionSpM(A, replicate(B, n)))

  }

  if (ncol.SpM(A) == 1) {

    n <- ncol.SpM(B)
    return(unionSpM(replicate(A, n), B))

  }

}

.union <- unionSpM

intersectionSpM <- function(x, y, proper = FALSE) {

  p <- as.integer(rep(0, dim.SpM(x)[2] + 1))
  i <- intersects_C(x$pp, x$pi - 1, dim.SpM(x),
                    y$pp, y$pi - 1, dim.SpM(y), p)

  M <- new_spm(i = i, p = p, nrow = dim.SpM(y)[2])

  return(M)

}

differenceSpM <- function(A, B) {

  # if (is.numeric(A)) A <- Matrix::Matrix(A, sparse = TRUE)
  # if (is.numeric(B)) B <- Matrix::Matrix(B, sparse = TRUE)

  applicable <- (ncol.SpM(A) == ncol.SpM(B)) ||
    (ncol.SpM(B) == 1) || (ncol.SpM(A) == 1)
  stopifnot(applicable)

  if (ncol.SpM(A) == ncol.SpM(B)) {

    A <- set_difference_SpM(A$pi - 1, A$pp, A$px,
                            B$pi - 1, B$pp, B$px,
                            nrow.SpM(A))

    return(A)

  }

  if (ncol.SpM(B) == 1) {

    n <- ncol.SpM(A)

    L <- set_difference_single_SpM(A$pi - 1, A$pp, A$px,
                                   B$pi - 1, B$pp, B$px,
                                   nrow.SpM(A))

    return(L)

    # return(new_spm(i = L$i, p = L$p, x = L$x,
    #                nrow = nrow.SpM(B)))

  }

  if (ncol.SpM(A) == 1) {

    n <- ncol.SpM(B)

    newA <- A %>% replicate(n)

    newA <- set_difference_SpM(newA$pi - 1, newA$pp, newA$px,
                               B$pi - 1, B$pp, B$px,
                               nrow.SpM(newA))

    return(newA)

  }

}


flattenSpM <- function(M) {

  # browser()

  S <- flatten_sparse_SpM(M$pp,
                        M$pi - 1,
                        M$px,
                        as.integer(dim.SpM(M)))

  return(S)

}

self_intersectSpM <- function(x, y) {

  self_intersection_C(x_i = x$pi - 1,
                      x_p = x$pp,
                      y_i = y$pi - 1,
                      y_p = y$pp)

}

equalSpM <- function(x, y = NULL) {

  if (is.null(y)) y <- x

  p <- as.integer(rep(0, dim.SpM(x)[2] + 1))
  i <- is_equal_set_C(as.integer(x$pp),
                      as.integer(x$pi - 1),
                      as.integer(dim.SpM(x)),
                      x$px,
                      as.integer(y$pp),
                      as.integer(y$pi - 1),
                      as.integer(dim.SpM(y)),
                      y$px,
                      as.logical(FALSE), p)

  new_spm(i = i + 1, p = p, nrow = ncol.SpM(y))

}

subsetSpM <- function(x, y = NULL, proper = FALSE) {

  if (is.null(y)) y <- x

  p <- as.integer(rep(0, dim.SpM(x)[2] + 1))
  xp <- as.integer(x$pp)
  xi <- as.integer(x$pi - 1)
  xd <- as.integer(dim.SpM(x))
  xx <- x$px
  yp <- as.integer(y$pp)
  yi <- as.integer(y$pi - 1)
  yd <- as.integer(dim.SpM(y))
  yx <- y$px

  i <- is_subset_C(xp,
                   xi,
                   xd,
                   xx,
                   yp,
                   yi,
                   yd,
                   yx,
                   as.logical(proper),
                   as.integer(p))

  new_spm(i = i + 1, p = p, nrow = dim.SpM(y)[2])

}

#' @importFrom rlang env_clone
cbindSpM <- function(...) {

  L <- list(...)

  L[sapply(L, is.null)] <- NULL

  res <- rlang::env_clone(L[[1]])
  L <- L[-1]
  for (i in L) {

    insert_columns(res, i)

  }

  class(res) <- "SpM"

  return(res)

}


#' @importFrom rlang env_has
extract_rows <- function(private, ids) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  idx <- lapply(private$pi_list,
                function(i) {

                  v <- match(ids, i)
                  v <- v[!is.na(v)]
                  v

                })

  new_pi <- lapply(seq_along(private$pi_list),
                   function(s)
                     private$pi_list[[s]][idx[[s]]])
  new_p <- c(0, cumsum(sapply(new_pi, length)))
  new_pi <- unlist(new_pi)
  new_px <- lapply(seq_along(private$px_list),
                   function(s)
                     private$px_list[[s]][idx[[s]]]) %>%
    unlist()

  names(ids) <- seq_along(ids)
  new_pi <- as.numeric(names(sort(ids)[new_pi]))

  dn <- private$dimnames
  if (length(dn[[1]]) > 0) {

    dn[[1]] <- dn[[1]][ids]

  }

  new_spm(i = new_pi, x = new_px, p = new_p,
          nrow = length(ids),
          dimnames = dn)

}

# order <- c(2, 1, 3, 4)
# pi <- c(2, 3, 4)
# should return c(1, 3, 4)
# reorder_rows <- function(private, order) {
#
#   if (!rlang::env_has(env = private, "pi_list")) {
#
#     L <- listing2(private)
#     assign("pi_list", L$pi_list, envir = private)
#     assign("px_list", L$px_list, envir = private)
#
#   }
#
#   private$pi_list <- lapply(private$pi_list, function(i) id[i])
#   private$px_list <- lapply(private$px_list, function(i) i[id])
#
#
# }

tSpM <- transposeSpM

#' @importFrom rlang env_has
whichSpM <- function(private) {

  if (!rlang::env_has(env = private, "pi_list")) {

    L <- listing2(private)
    assign("pi_list", L$pi_list, envir = private)
    assign("px_list", L$px_list, envir = private)

  }

  i <- private$pi_list
  p <- private$pp
  n <- private$pnrow

  j <- seq_along(p[-length(p)])
  idx <- j %>%
    sapply(function(jj) i[[jj]] + (jj - 1) * n) %>%
    unlist()

  return(idx)

}

rowSums <- function(A) {

  i <- A$pi
  x <- A$px
  rows <- seq(A$pnrow)
  res <- sapply(rows, function(r) {

    sum(x[i == r])

  })

  if (length(A$dinnames) > 0)
    names(res) <- A$dimnames[[1]]

  return(res)

}

#' @importFrom rlang env_has
zero_rows <- function(M, idx) {

  if (!rlang::env_has(env = M, "pi_list")) {

    L <- listing2(M)
    assign("pi_list", L$pi_list, envir = M)
    assign("px_list", L$px_list, envir = M)

  }

  idxs <- lapply(L$pi_list, function(ii) which(!(ii %in% idx)))
  pi_list <- lapply(seq_along(M$pi_list), function(ii) M$pi_list[[ii]][idxs[[ii]]])
  px_list <- lapply(seq_along(M$px_list), function(ii) M$px_list[[ii]][idxs[[ii]]])

  M$pi_list <- pi_list
  M$px_list <- px_list
  M$pi <- unlist(M$pi_list)
  M$px <- unlist(M$px_list)
  M$pp <- c(0, cumsum(sapply(M$pi_list, length)))

}

assignSpM <- function(private, idx, values) {

  if (length(values) == 1) {

    values <- rep(values, length(idx))

  }

  i <- private$pi
  x <- private$px

  idx_yes <- idx %in% i
  idx_no <- !idx_yes

  x[idx[idx_yes]] <- values[idx_yes]

  i <- c(i, idx[idx_no])
  x <- c(x, values[idx_no])

  o <- order(i)
  private$pi <- i[o]
  private$px <- x[o]
  private$pp <- c(0, length(i))

}

dimnamesSpM <- function(private) {

  private$dimnames

}

assign_dimnamesSpM <- function(private, value) {

  private$dimnames <- value

}

colnamesSpM <- function(private) {

  private$dimnames[[2]]

}

assign_colnamesSpM <- function(private, value) {

  dn <- private$dimnames
  dn[[2]] <- value
  private$dimnames <- dn

}

rownamesSpM <- function(private) {

  private$dimnames[[1]]

}

assign_rownamesSpM <- function(private, value) {

  dn <- private$dimnames
  dn[[1]] <- value
  private$dimnames <- dn

}

andSpM <- function(A, B) {

  # A and B should have the same
  # number of columns and rows
  n <- ncol.SpM(A)

  j <- seq(n)
  dpA <- diff(A$pp)
  jjA <- rep(j, times = dpA)

  dpB <- diff(B$pp)
  jjB <- rep(j, times = dpB)

  iiA <- (jjA - 1) * A$pnrow + A$pi
  iiB <- (jjB - 1) * B$pnrow + B$pi

  idx <- which(iiA %in% iiB)
  new_i <- A$pi[idx]
  new_j <- jjA[idx]

  dp <- sapply(j, function(x)
    length(which(new_j == x)))

  new_p <- c(0, cumsum(dp))

  new_spm(i = new_i,
          p = new_p,
          x = rep(1, length(new_i)),
          nrow = A$pnrow)

}
