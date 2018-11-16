# Principal Component Analysis (PCA)


#' PCA
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @param k Number of eigenvalues requested.
#' @param seed A numeric number of seed used for Capper's method of
#'   determining PC number. In brief, the method shuffles features
#'   across samples and determine the PC number by comparing the
#'   maximum of eigen values from the randomization.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance to choose PC number. Default to 0.9.
#' @return A list of four elements:
#'   \itemize{
#'     \item \code{projected} The result matrix of PCA analysis.
#'     \item \code{pca123} A list of result from step 1-3 returned by
#'       \code{\link{pca123}}.
#'     \item \code{capper} A list of the result of choosing PC number
#'       by Capper's method, returned by \code{\link{find_pc_number.capper}}.
#'     \item \code{vf} A list of the result of choosing PC number
#'       by fraction of variance, returned by \code{\link{find_pc_number.var_frac}}.
#'   }
#' @details The function wraps up the following five steps of PCA:
#'    \enumerate{
#'     \item Center and scale.
#'     \item Compute the correlation/covariance matrix.
#'     \item Calculate the eigenvectors and eigenvalues.
#'     \item Choose the PC number. I use Capper's method and fraction of
#'       variance to calculate PC numbers and choose the bigger one
#'       from the two methods. See details at \code{\link{find_pc_number.capper}}
#'       and \code{\link{find_pc_number.var_frac}}.
#'     \item Project the scaled input matrix onto the new basis.
#'   }
#'   I use \code{\link[RSpectra]{eigs}} function in Rspectra package
#'   instead of \code{\link[base]{eigen}} function in base to deal
#'   with large matrix.
#' @export
pca <- function(x, k = 50, seed = 1, threshold = 0.9) {
  # Step 1-3
  res <- pca123(x, k = k)
  # Step 4 Choose principal components (PCs).
  set.seed(seed = seed)
  capper_res <- find_pc_number.capper(x, res$eigs$values)
  vf_res <- find_pc_number.var_frac(res$eigs$values, threshold)
  pc_num <- max(capper_res$pc_num, vf_res$pc_num)
  x_projected <- project_pc(pca123_res = res, pc_num = pc_num)
  list(
    pca123 = res,
    capper = capper_res,
    vf = vf_res,
    projected = x_projected
  )
}


#' PCA step 1, 2, 3.
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
#' @details PCA steps 1, 2, 3 are:
#'    \enumerate{
#'     \item Center and scale.
#'     \item Compute the correlation/covariance matrix.
#'     \item Calculate the eigenvectors and eigenvalues.
#'   }
#'   I use \code{\link[RSpectra]{eigs}} function in Rspectra package
#'   instead of \code{\link[base]{eigen}} function in base to deal
#'   with large matrix.
pca123 <- function(x, k = 50) {
  # Step 1 Center and scale.
  x_scaled <- scale(x, center = TRUE, scale = TRUE)
  # Step 2 Compute the correlation/covariance matrix.
  x_scaled_cor <- cov(x_scaled)
  # Step 3 Calculate the eigenvectors and eigenvalues of the covariance
  # matrix. Top k is requested.
  # Notice: I use eigs() function in Rspectra package instead of
  # eigen() function in base because the matrix is large.
  x_eig <- RSpectra::eigs(x_scaled_cor, k = k)
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


#' Project to new basis.
#'
#' Project the scaled input matrix (i.e. z-scores) onto the new basis.
#'
#' @param pca123_res A list returned by \code{\link{pca123}}.
#' @param pc_num An integer scalar of PC number.
#' @return A matrix which has columns as new features (PCs) and rows
#'   as samples.
project_pc <- function(pca123_res, pc_num) {
  if (missing(pca123_res))
    stop("Argument pca123_res is required.")
  if (missing(pc_num))
    stop("Argument pc_num is required.")
  phi <- pca123_res$eigs$vectors[, seq(pc_num)]
  row.names(phi) <- colnames(pca123_res)
  colnames(phi) <- paste0("PC", seq(pc_num))
  pca123_res$scaled %*% phi
}


#' Shuffle matrix.
#'
#' Each row or column is shuffled separately. To reproduce the result,
#' set the seed before running the function.
#'
#' @param x A matrix.
#' @param margin A character scalar, either "row" or "column". Default
#'   to "column", because columns are usually used as features.
#' @return A matrix.
#' @export
shuffle_matrix <- function(x, margin = c("column", "row")) {
  if (missing(x))
    stop("Argument x is required.")
  if (!is.matrix(x))
    stop("Argument x should be a matrix.")
  margin <- match.arg(margin)
  switch(margin,
    column = shuffle_matrix.column(x),
    row = shuffle_matrix.row(x)
  )
}



#' Shuffle matrix by column.
#'
#' Each column is shuffled separately. To reproduce the result, set the
#' seed before running the function.
#'
#' @param x A matrix.
#' @return A matrix.
#' @noRd
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


#' Find PC number: fraction of variance.
#'
#' The fraction of total variance retained for each PC is calculated by
#' dividing each eigen value by the total sum of eigen values. The
#' assumption behind the approach is that high variance surrogates
#' high importance, which may be not true in the real data.
#'
#' @param eigen_values A vector of eigen values. It is the item
#'   \code{values} of the item \code{eigs} of the returned value of
#'   \code{\link{pca123}}, which is returned by
#'   \code{\link[RSpectra]{eigs}}.
#' @param threshold A numeric scalar between 0 to 1 of the threshold of
#'   the fraction of variance. Default to 0.9.
#' @return Depends on which method is called.
#' @export
find_pc_number.var_frac <- function(eigen_values, threshold = 0.9) {
  if (missing(eigen_values))
    stop("Argument eigen_values is required.")
  if (!is.numeric(eigen_values))
    stop("Argument eigen_values should be a numeric vector.")
  if (threshold < 0 | threshold > 1)
    stop("Argument threshold should between 0 to 1.")
  frac_var <- cumsum(eigen_values) / sum(eigen_values)
  pc_num <- min(which(frac_var > threshold))

  data.frame(x = seq(length(frac_var)), y = frac_var) %>%
    ggplot2::ggplot(., ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_path(colour = "royalblue") +
    ggplot2::geom_point(colour = "royalblue") +
    ggplot2::geom_vline(xintercept = pc_num,
                        colour = "coral",
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = threshold,
                        colour = "coral",
                        linetype = "dashed") -> p
  list(pc_num = pc_num, variance_fraction = frac_var, plot = p)
}


#' Find PC number: Capper's method.
#'
#' The method is described in the paper by Capper et al. entitled
#' "DNA methylation-based classification of central nervous system
#' tumours". In brief, the method shuffles features across samples
#' and determine the PC number by comparing the maximum of eigen
#' values from the randomization.
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @param eigen_values A vector of eigen values. It is the item
#'   \code{values} of the item \code{eigs} of the returned value of
#'   \code{\link{pca123}}, which is returned by
#'   \code{\link[RSpectra]{eigs}}.
#' @return A list of three items:
#'   \itemize{
#'     \item \code{pc_num} An integer scalar of PC number.
#'     \item \code{variance_fraction} The sum of fraction of variance
#'       of \code{pc_num} PCs.
#'    \item \code{eigs_shuffled} A numeric vector of eigen values of
#'       the randomized data.
#'     \item \code{plot} A \code{\link[ggplot2]{ggplot}} object of
#'       density plot of eigenvalues of observed and randomized data.
#'   }
#' @export
find_pc_number.capper <- function(x, eigen_values) {
  .check_args_find_pc_number(x, eigen_values)
  k <- length(eigen_values)
  x_shuffled <- shuffle_matrix(x, margin = "col")
  res_shuffled <- pca123(x_shuffled, k = k)
  output <- list()
  # Number of PCs whose eigenvalues are larger the maximum of the
  # randomized data.
  pc_num <- sum(eigen_values > max(res_shuffled$eigs$values))
  frac_var <- cumsum(eigen_values) / sum(eigen_values)
  output$eigs_shuffled <- res_shuffled$eigs$values
  output$pc_num <- pc_num
  output$variance_fraction <- frac_var[pc_num]
  # Plot
  data.frame(observed = eigen_values,
             randomized = res_shuffled$eigs$values) %>%
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
      xintercept = max(res_shuffled$eigs$values),
      linetype = "dashed",
      color = "royalblue"
    ) -> output$plot
   output
}


#' Find PC number: yamat method.
#'
#' The method is based upon Capper's method (see \code{\link{find_pc_number.capper}}).
#' It estimates the number of the maximum eigenvalue of randomized data by
#' repeat the randomization for several times.
#'
#' @param x A matrix which has columns as features and rows as samples.
#' @param eigen_values A vector of eigen values. It is the item
#'   \code{values} of the item \code{eigs} of the returned value of
#'   \code{\link{pca123}}, which is returned by
#'   \code{\link[RSpectra]{eigs}}.
#' @param n An integer scalar of randomization times. Default to 30.
#' @return A list of three items:
#'   \itemize{
#'     \item \code{pc_num} An integer scalar of PC number.
#'     \item \code{variance_fraction} The sum of fraction of variance
#'       of \code{pc_num} PCs.
#'     \item \code{raw} A \code{data.frame} of maximum eigen values
#'       and PC numbers.
#'   }
#' @note The Capper and yamat methods gives the same number of PCs
#'   on solid tumor example.
#' @export
find_pc_number.yamat <- function(x, eigen_values, n = 30) {
  .check_args_find_pc_number(x, eigen_values)
  lapply(
    seq(n),
    function(i) {
      res <- find_pc_number.capper(x, eigen_values)
      data.frame(pc_num = res$pc_num,
                 max_eigs = max(res$eigs_shuffled))
    }
  ) %>%
    do.call(rbind, .) -> trials
  pc_num <- sum(eigen_values > mean(trials$max_eigs))
  frac_var <- cumsum(eigen_values) / sum(eigen_values)
  output <- list()
  output$pc_num <- pc_num
  output$variance_fraction <- frac_var[pc_num]
  output$raw <- trials
  output
}


.check_args_find_pc_number <- function(x, eigen_values) {
  if (missing(x))
    stop("Argument x is required.")
  if (!is.matrix(x))
    stop("Argument x should be a matrix.")
  if (missing(eigen_values))
    stop("Argument eigen_values is required.")
  if (!is.numeric(eigen_values))
    stop("Argument eigen_values should be a numeric vector.")
}

