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
