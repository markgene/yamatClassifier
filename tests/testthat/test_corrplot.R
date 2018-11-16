context("Corrplot")
library(yamatClassifier)

x <- as.matrix(mtcars[, c(1, 3:6)])

test_that(
  "correlogram()", {
    expect_error(correlogram(x = x, pheno = mtcars$cyl), NA)
  }
)


if (file.exists("Rplots.pdf"))
  file.remove("Rplots.pdf")
