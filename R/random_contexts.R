#' @useDynLib fcaR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Generate Random Formal Contexts
#'
#' @description
#' Functions to generate synthetic formal contexts using advanced statistical distributions.
#' These methods allow creating datasets that mimic real-world properties (non-uniform density)
#' or randomizing existing contexts while preserving their structural properties.
#'
#' @param n_objects (integer) Number of objects.
#' @param n_attributes (integer) Number of attributes.
#' @param density (numeric) Expected density of the context (proportion of 1s). Used for uniform distribution.
#' @param distribution (character) The distribution to use for generating the context.
#' \itemize{
#'   \item \code{"uniform"}: Each cell is 1 with probability \code{density}.
#'   \item \code{"dirichlet"}: The number of attributes per object follows a categorical distribution
#'   derived from a Dirichlet distribution. This creates "clumpy" or "sparse" rows typical of real data.
#' }
#' @param alpha (numeric) Concentration parameter for the Dirichlet distribution.
#' Low values (e.g., 0.1) produce very skewed distributions (some objects have few attributes, others many).
#' High values produce more uniform row sums. Default is 1.0.
#' @param ... Additional arguments passed to internal methods.
#'
#' @return A \code{FormalContext} object.
#'
#' @examples
#' # 1. Uniform Random Context
#' fc_uni <- RandomContext(10, 5, density = 0.2)
#' print(fc_uni)
#'
#' # 2. Dirichlet Random Context (Mimicking real data structure)
#' # Objects will have varying 'sizes' (number of attributes)
#' fc_dir <- RandomContext(10, 5, distribution = "dirichlet", alpha = 0.5)
#' print(fc_dir)
#'
#' @importFrom stats rgamma
#'
#' @export
RandomContext <- function(n_objects, n_attributes, density = 0.1, distribution = "uniform", alpha = 1.0, ...) {
  # Validate input
  if (n_objects < 1 || n_attributes < 1) stop("Dimensions must be positive.")

  I <- matrix(0, nrow = n_objects, ncol = n_attributes)

  if (distribution == "uniform") {
    # Bernoulli trials for each cell
    n_ones <- round(n_objects * n_attributes * density)
    indices <- sample(n_objects * n_attributes, n_ones)
    I[indices] <- 1
  } else if (distribution == "dirichlet") {
    # Inspired by conexp-clj:
    # 1. Generate a probability vector p from Dirichlet(alpha)
    #    where p[k] is the prob that an object has exactly k attributes.
    # 2. Sample row sums for each object from this distribution.

    # Since we don't want to add 'gtools' dependency just for rdirichlet,
    # we implement it using Gamma(alpha, 1).
    # X_i ~ Gamma(alpha, 1) => X / sum(X) ~ Dirichlet(alpha)

    # We simulate a distribution over 0..n_attributes (possible counts)
    # Base measure is uniform (1,1,...,1) scaled by alpha as in conexp-clj
    # Actually, conexp-clj uses a base measure vector. We simplify to scalar alpha.

    k_levels <- n_attributes + 1 # 0 to n_attributes

    # Sample from Gamma
    gamma_samples <- rgamma(k_levels, shape = alpha, rate = 1)
    prob_vector <- gamma_samples / sum(gamma_samples)

    # Sample row sums (how many attributes each object has)
    row_sums <- sample(0:n_attributes, n_objects, replace = TRUE, prob = prob_vector)

    # Fill matrix
    for (i in 1:n_objects) {
      n_attrs_for_obj <- row_sums[i]
      if (n_attrs_for_obj > 0) {
        chosen_attrs <- sample(n_attributes, n_attrs_for_obj)
        I[i, chosen_attrs] <- 1
      }
    }
  } else {
    stop("Unknown distribution. Supported: 'uniform', 'dirichlet'.")
  }

  # Assign names
  rownames(I) <- paste0("O", seq_len(n_objects))
  colnames(I) <- paste0("A", seq_len(n_attributes))

  return(FormalContext$new(I))
}

#' Randomize an Existing Formal Context
#'
#' @description
#' Modifies the incidence matrix of a formal context to create a random variation
#' while preserving certain statistical properties. This is essential for statistical
#' significance testing in FCA (e.g., "is this concept structure random?").
#'
#' @param fc (\code{FormalContext}) The context to randomize.
#' @param method (character) The randomization strategy:
#' \itemize{
#'   \item \code{"swap"}: Edge Swapping (Curveball algorithm). Preserves exact row sums and column sums (marginal distributions). The structure changes, but the statistics of objects and attributes remain identical.
#'   \item \code{"rewire"}: Edge Rewiring. Preserves only the global density (total number of 1s). Row and column sums may change.
#' }
#' @param iterations (integer) Number of swap/rewire operations to perform.
#' Default is \code{10 * number of 1s}, which is usually sufficient for mixing.
#'
#' @return A new \code{FormalContext} object with the randomized incidence.
#'
#' @examples
#' data(planets)
#' fc <- FormalContext$new(planets)
#'
#' # 1. Edge Swapping (Preserves degree distribution)
#' # Useful for null-model testing
#' fc_rand_swap <- randomize_context(fc, method = "swap")
#'
#' # Verify marginals are preserved
#' colSums(fc$incidence())
#' colSums(fc_rand_swap$incidence())
#'
#' # 2. Rewiring (Preserves only density)
#' fc_rand_rewire <- randomize_context(fc, method = "rewire")
#'
#' @export
randomize_context <- function(fc, method = "swap", iterations = NULL) {
  if (!inherits(fc, "FormalContext")) stop("Input must be a FormalContext.")

  # Convert to Dense IntegerMatrix for C++ processing
  # Note: We use as.matrix() here because randomization is an intensive
  # matrix operation, usually done on small-medium matrices for statistical testing.
  I <- as.matrix(fc$incidence())

  # Ensure binary (convert fuzzy to binary 0/1 based on >0)
  I_bin <- matrix(0L, nrow = nrow(I), ncol = ncol(I))
  I_bin[I > 0] <- 1L

  if (is.null(iterations)) {
    iterations <- sum(I_bin) * 10
  }

  I_new <- NULL

  if (method == "swap") {
    I_new <- randomize_swap_cpp(I_bin, iterations)
  } else if (method == "rewire") {
    I_new <- randomize_rewire_cpp(I_bin, iterations)
  } else {
    stop("Unknown method. Supported: 'swap', 'rewire'.")
  }

  # Create new context preserving names
  rownames(I_new) <- rownames(I)
  colnames(I_new) <- colnames(I)

  return(FormalContext$new(I_new))
}

#' Generate a Random Distributive Context
#'
#' @description
#' Generates a random formal context that is guaranteed to produce a
#' **Distributive Concept Lattice**.
#'
#' It relies on Birkhoff's Representation Theorem: The lattice of order ideals
#' of a Poset is always distributive. The context is constructed such that
#' objects and attributes are the elements of the poset, and the incidence
#' relation is \eqn{g I m \iff \neg(g \ge m)}.
#'
#' @param n_elements Number of elements in the underlying Poset.
#' @param density Probability of an order relation \eqn{a \le b}.
#'
#' @return A \code{FormalContext}.
#' @export
RandomDistributiveContext <- function(n_elements, density = 0.1) {

  # 1. Generar Matriz de Adyacencia Aleatoria (DAG)
  # M[i, j] = 1 significa i <= j
  M <- matrix(0, nrow = n_elements, ncol = n_elements)

  # Solo llenamos triángulo superior para evitar ciclos (i < j)
  idx <- which(upper.tri(M))
  n_edges <- round(length(idx) * density)
  if(n_edges > 0) {
    M[sample(idx, n_edges)] <- 1
  }

  # 2. Reflexividad (i <= i)
  diag(M) <- 1

  # 3. Cierre Transitivo (Warshall) para tener un Poset válido
  # Si i <= k y k <= j => i <= j
  for (k in 1:n_elements) {
    # Vectorización parcial para velocidad en R
    # M[i, j] = M[i, j] | (M[i, k] & M[k, j])
    # Iteramos filas i que tienen conexión con k
    i_connected_to_k <- which(M[, k] == 1)
    if (length(i_connected_to_k) > 0) {
      k_connected_to_j <- which(M[k, ] == 1)
      if (length(k_connected_to_j) > 0) {
        # Expandir cuadrícula
        M[i_connected_to_k, k_connected_to_j] <- 1
      }
    }
  }

  # 4. Construir Contexto para el Retículo de Ideales
  # Relación: g I m <=> NO (g >= m)
  # g >= m significa M[m, g] == 1 (m es menor o igual que g)
  # Por tanto: I[g, m] = 1 - M[m, g]

  I <- 1 - t(M)

  # Asignar nombres
  rownames(I) <- paste0("E", 1:n_elements)
  colnames(I) <- paste0("E", 1:n_elements)

  return(FormalContext$new(I))
}
