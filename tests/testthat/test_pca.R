context("PCA")
library(yamatClassifier)

x <- as.matrix(mtcars[, c(1, 3:6)])
k <- 3

test_that(
  "pca123()", {
    expect_error(res <- pca123(x = x, k = k), NA)
  }
)

test_that(
  "shuffle_matrix.column()", {
    expect_error(x_shuffled <- shuffle_matrix.column(x), NA)
    expect_true(all(x_shuffled[, 1] %in% x[, 1]))
  }
)

test_that(
  "shuffle_matrix.row()", {
    expect_error(x_shuffled <- shuffle_matrix.row(x), NA)
    expect_true(all(x_shuffled[1, ] %in% x[1, ]))
  }
)

test_that(
  "shuffle_matrix()", {
    expect_error(x_shuffled <- shuffle_matrix(x, "column"), NA)
    expect_true(all(x_shuffled[, 1] %in% x[, 1]))
    expect_error(x_shuffled <- shuffle_matrix(x, "row"), NA)
    expect_true(all(x_shuffled[1, ] %in% x[1, ]))
  }
)

