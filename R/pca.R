# Principal Component Analysis (PCA)


#' PCA
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @return TBA.
pca <- function(x, k = 50, seed = 1) {
  res <- .pca123(x, k = k)
  # Step 4 Choose principal components (PCs). Following Capper's paper,
  # I determine the number of PCs by comparing eigenvalues to the
  # maximum eigenvalue of a PCA using randomize beta values, which is
  # obtained by shuffing sample labels per loci.
  # Randomize beta values.
  set.seed(seed = seed)
  lapply(
    seq(nrow(beta_vals_mv)),
    function(i) {
      sample(beta_vals_mv[i, ])
    }
  ) %>%
    do.call(rbind, .) %>%
    as.matrix() %>%
    t() %>%
    cov() -> loci_cov_randomized

  # Calculate eigenvalues of randomized beta values.
  loci_eig_randomized <- eigs(loci_cov_randomized, k = 50)

  data.frame(ob = loci_eig$values, bg = loci_eig_randomized$values) %>%
    tidyr::gather(key = "type", value = "eigenvalue") %>%
    ggpubr::ggdensity(
      .,
      "eigenvalue",
      xlab = "PC eigenvalue",
      ylab = "Density",
      color = "type",
      fill = "type",
      alpha = 0.6,
      palette = "jco"
    ) +
    ggplot2::geom_vline(
      xintercept = max(loci_eig_randomized$values),
      linetype = "dashed",
      color = "royalblue"
    )

  pca_num <- ceiling(max(loci_eig_randomized$values))
}


#' PCA step 1, 2, 3
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @param k Number of eigenvalues requested.
#' @return A list of four elements:
#'   \itemize{
#'     \item \code{scaled} A matrix of scaled input matrix.
#'     \item \code{cor_mat} Correlation/covariance matrix of the scaled
#'       matrix.
#'     \item \code{eigs} A list returned by \code{\link[RSpectra]{eigs}}.
#'     \item \code{plot} A \code{\link[ggplot2]{ggplot}} object of
#'       density plot of eigenvalues.
#'   }
pca123 <- function(x, k = 50) {
  # Step 1 Center and scale.
  x_scaled <- scale(x, center = TRUE, scale = TRUE)
  # Step 2 Compute the correlation/covariance matrix.
  x_scaled_cor <- cov(x_scaled)
  # Step 3 Calculate the eigenvectors and eigenvalues of the covariance
  # matrix. Top k is requested.
  # Notice: I use eigs() function in Rspectra package instead of
  # eigen() function in base because the matrix is large.
  x_eig <- eigs(x_scaled_cor, k = k)
  ggpubr::ggdensity(
    data.frame(x = x_eig$values),
    "x",
    xlab = "PC eigenvalue",
    ylab = "Density"
  ) -> eig_density_plot
  list(scaled = x_scaled,
       cor_mat = x_scaled_cor,
       eigs = x_eig,
       plot = eig_density_plot)
}


#' Shuffle matrix by column.
#'
#' Each column is shuffled separately. To reproduce the result, set the
#' seed before running the function.
#'
#' @param x A matrix.
#' @return A matrix.
shuffle_matrix.column <- function(x) {
  lapply(
    seq(ncol(x)),
    function(i) {
      sample(x[, i])
    }
  ) %>%
    do.call(rbind, .) %>%
    as.matrix() %>%
    t() -> x_shuffled
}


#' Shuffle matrix by row.
#'
#' Each row is shuffled separately. To reproduce the result, set the
#' seed before running the function.
#'
#' @param x A matrix.
#' @return A matrix.
#' @noRd
shuffle_matrix.row <- function(x) {
  lapply(
    seq(nrow(x)),
    function(i) {
      sample(x[i, ])
    }
  ) %>%
    do.call(rbind, .) %>%
    as.matrix() -> x_shuffled
}