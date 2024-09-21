# Most variably methylated loci.


#' Most variably methylated loci.
#'
#' Obtain the most variably methylated loci. The variability is measured
#' by standard deviation. The function can also meet the general need to
#' obtaint the most variable rows.
#'
#' @param x A matrix of beta values.
#' @param top_n An integer of top N probes. Default to 1000.
#' @return A matrix of beta values of most variably methylated loci.
#' @export
most_variable <- function(x, top_n = 1000) {
  if (missing(x)) {
    stop("Require argument x.")
  }
  row_sd <- matrixStats::rowSds(x)
  output <- utils::head(x[order(row_sd, decreasing = TRUE), ], n = top_n)
  return(output)
}


#' Plot variablity of methylation signals across loci.
#'
#' Plot the distribution of standard deviation of methylation across loci.
#' If \code{top_n} is specified, add vertical line to denote where the
#' minimum of SD of the top variably methylated locus is.
#'
#' @param x A matrix of beta values.
#' @param top_n An integer of top N probes. Default to 1000.
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
density_plot_row_sd <- function(x, top_n = 1000) {
  row_sd <- matrixStats::rowSds(x)
  cutoff <- min(head(sort(row_sd, decreasing = TRUE), n = top_n))
  data.frame(x = row_sd) %>%
    ggpubr::ggdensity(
      .,
      x = "x",
      fill = "lightgray",
      xlab = "SD",
      ylab = "Density",
      title = "Distribution of SDs of beta values across loci."
    ) +
    ggplot2::geom_vline(xintercept = cutoff,
                        linetype = "dashed",
                        colour = "red")
}
