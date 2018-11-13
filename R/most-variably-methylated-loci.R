#' Most variably methylated loci.
#'
#' Obtain the most variably methylated loci. The variability is measured
#' by standard deviation.
#'
#' @param x A matrix of beta values.
#' @param top_n An integer of top N probes. Default to 1000.
#' @return A matrix of beta values of most variably methylated loci.
#' @export
most_variably_methylated_loci <- function(x, top_n = 1000) {
  if (missing(x))
    stop("Require argument x.")
  beta_sd <- rowSds(x)
  names(beta_sd) <- rownames(x)
  keep <- rownames(beta_vals) %in% names(head(sort(beta_sd, decreasing = TRUE), top_n))
  x[keep, ]
}
