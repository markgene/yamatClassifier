# tSNE

#' Run tSNE in batch mode.
#'
#' @param x A matrix.
#' @param n An integer scalar. Default to 10.
#' @param ... Any arguments passed to \code{\link[RTsne]{RTsne}}.
#' @return A list of the returned value of \code{\link[RTsne]{RTsne}}.
tsne <- function(x, n = 10, ...) {
  lapply(seq(n), function(i) {
    Rtsne::Rtsne(X = x, ...)
  })
}


#' Plot tSNE result.
#'
#' @param tsne_res The returned value of \code{\link[RTsne]{RTsne}},
#'   which is a list.
#' @param ... Any arguments passed to \code{\link[ggplot2]{aes}}
#' @return A \code{\link[ggplot2]{ggplot}} object.
plot_tsne <- function(tsne_res, ...) {
  tsne_res$Y %>%
    as.data.frame() %>%
    ggplot2::ggplot(., ggplot2::aes(
      x = V1,
      y = V2,
      ...
    )) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "", y = "", colour = "Phenotype") +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank()) +
    ggplot2::scale_colour_brewer(palette = "Set2")
}

