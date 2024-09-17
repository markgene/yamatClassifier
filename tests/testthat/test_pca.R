context("Test pca()")
library(yamatClassifier)

x <- as.matrix(mtcars[, c(1, 3:6)])
k <- 3

test_that(
  "pca()", {
    expect_error(res <- pca(x = x, k = k, seed = 1), NA)
  }
)

test_that(
  "pca123()", {
    expect_error(res <- pca123(x = x, k = k), NA)
  }
)

test_that("find_pc_number.var_frac()", {
  res <- pca123(x = x, k = k)
  set.seed(1)
  expect_error(find_pc_number.var_frac(eigen_values = res$eigs$values),
               NA)
})

test_that("find_pc_number.capper()", {
  res <- pca123(x = x, k = k)
  set.seed(1)
  expect_error(pc_capper <-
                 find_pc_number.capper(x = x,
                                       eigen_values = res$eigs$values),
               NA)
  expect_true(pc_capper$variance_fraction - 0.8314244 < 1e-7)
})

test_that("find_pc_number.yamat()", {
  res <- pca123(x = x, k = k)
  set.seed(1)
  expect_error(find_pc_number.yamat(
    x = x,
    eigen_values = res$eigs$values,
    n = 3
  ),
  NA)
})

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

