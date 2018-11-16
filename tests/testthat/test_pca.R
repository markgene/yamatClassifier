context("PCA")
library(yamatClassifier)

test_that(
  "shuffle_matrix.column()", {
    x <- as.matrix(mtcars[, 1:4])
    expect_error(x_shuffled <- shuffle_matrix.column(x), NA)
    expect_true(all(x_shuffled[, 1] %in% x[, 1]))
  }
)

test_that(
  "shuffle_matrix.row()", {
    x <- as.matrix(mtcars[, 1:4])
    expect_error(x_shuffled <- shuffle_matrix.row(x), NA)
    expect_true(all(x_shuffled[1, ] %in% x[1, ]))
  }
)

test_that(
  "shuffle_matrix()", {
    x <- as.matrix(mtcars[, 1:4])
    expect_error(x_shuffled <- shuffle_matrix(x, "column"), NA)
    expect_true(all(x_shuffled[, 1] %in% x[, 1]))
    expect_error(x_shuffled <- shuffle_matrix(x, "row"), NA)
    expect_true(all(x_shuffled[1, ] %in% x[1, ]))
  }
)

