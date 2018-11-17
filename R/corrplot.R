# Customized correlogram


#' Correlogram.
#'
#' @param x A matrix.
#' @param ... Any arguments passed to \code{\link[corrplot]{corrplot}}.
#' @return (Invisibly) returns a reordered correlation matrix.
#' @export
correlogram <- function(x, pheno, ...) {
  # Reorder
  M <- cor(x)
  # dd <- as.dist((1 - M) / 2)
  dd <- dist(x)
  hc <- hclust(dd)
  pheno <- pheno[hc$order]
  M <- M[hc$order, hc$order]
  x <- x[, hc$order]
  # Calculate p-value matrix.
  p.mat <- cor_mtest(x)
  # Color
  col <-
    grDevices::colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  pal <- .custom_pal(length(unique(pheno)))
  # Rename sample to pheno
  rownames(M) <- pheno
  # Plot.
  corrplot::corrplot(
    M,
    method = "color",
    col = col(200),
    order = "original",
    tl.col = pal[pheno],
    tl.pos = "l",
    # Combine with significance
    p.mat = p.mat,
    sig.level = 0.01,
    insig = "pch",
    pch.cex = 0.5,
    pch.col = "grey70",
    # hide correlation coefficient on the principal diagonal
    diag = TRUE
  )
}


.custom_pal <- function(n, k = 8, set = "Dark2") {
  if (n < k)
    return(RColorBrewer::brewer.pal(n, set))
  else {
    colorRampPalette(brewer.pal(k, set))(n)
  }
}

#' Computing the p-value of correlations.
#'
#' @param x A matrix.
#' @param ... Any arguments to pass to \code{\link[stats]{cor.test}}
#'   function.
#' @return A matrix of p-values.
#' @export
#' @note I borrow most of the codes from \href{http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram}{STHDA}.
cor_mtest <- function(x, ...) {
  x <- as.matrix(x)
  n <- ncol(x)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  sapply(
    seq(as.integer(n - 1)),
    function(i) {
      sapply(
        seq(from = as.integer(i + 1), to = n, by = 1),
        function(j) {
          tmp <- cor.test(x[, i], x[, j], ...)
          p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        }
      )
    }
  )
  colnames(p.mat) <- rownames(p.mat) <- colnames(x)
  p.mat
}

