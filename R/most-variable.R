#' Most variably methylated loci.
#'
#' Obtain the most variably methylated loci. The variability is measured
#' by standard deviation.
#'
#' @param x A matrix of beta values.
#' @param top_n An integer of top N probes. Default to 1000.
#' @return A matrix of beta values of most variably methylated loci.
#' @export
most_variable <- function(x, top_n = 1000) {
  if (missing(x))
    stop("Require argument x.")
  row_sd <- matrixStats::rowSds(x)
  head(x[order(row_sd, decreasing = TRUE), ], n = top_n)
}
