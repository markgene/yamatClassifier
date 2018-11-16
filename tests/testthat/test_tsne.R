context("tSNE")
library(yamatClassifier)

x <- as.matrix(mtcars[, c(1, 3:6)])

test_that(
  "tsne()", {
    expect_error(res <- tsne(x = x, n = 3, perplexity = 2), NA)
  }
)

test_that(
  "plot tSNE", {
    res <- tsne(x = x, n = 3, perplexity = 2)
    expect_error(plot_tsne(res[[1]]), NA)
    expect_error(multiplot_tsne(res, pheno = mtcars$cyl), NA)
  }
)
