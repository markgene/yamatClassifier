context("Test train_batch_effect_model()")
library(ggplot2)
library(yamatClassifier)

get_test_data <- function() {
  set.seed(123)
  x <- matrix(rnorm(10 * 18), 10, 18)
  x[, 1:3] <- x[, 1:3] + 5
  batch <- rep(c("A", "B", "C"), each = 6)
  batch2 <- rep(c("D", "E"), 9)
  batch <- as.factor(batch)
  batch2 <- as.factor(batch2)
  return(list(
    x = x,
    batch = batch,
    batch2 = batch2
  ))
}


test_that("train_batch_effect_model()", {
  dat <- get_test_data()
  yamatClassifier::train_batch_effect_model(
    x = dat$x,
    batch = dat$batch,
    batch2 = dat$batch2,
    log2_transform = FALSE,
    adjusted_rda = "output/batch_effect_adjusted.Rda",
    fit_rda = "output/batch_effect_fit.Rda"
  )
  expect_true(file.exists("output/batch_effect_adjusted.Rda"))
  expect_true(file.exists("output/batch_effect_fit.Rda"))
})


test_that("train_batch_effect_model() with log2 transform", {
  dat <- get_test_data()
  yamatClassifier::train_batch_effect_model(
    x = (dat$x) ^ 2,
    batch = dat$batch,
    batch2 = dat$batch2,
    log2_transform = FALSE,
    adjusted_rda = "output/batch_effect_adjusted.Rda",
    fit_rda = "output/batch_effect_fit.Rda"
  )
  expect_true(file.exists("output/batch_effect_adjusted.Rda"))
  expect_true(file.exists("output/batch_effect_fit.Rda"))
})
