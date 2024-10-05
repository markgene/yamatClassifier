context("Test train_model1()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("train_model1() end-to-end", {
  require(datasets)
  utils::data(iris)
  tune_result <- yamatClassifier::train_model1(
    dat = iris,
    response_name = "Species",
    outer_cv_folds = 3,
    inner_cv_folds = 3,
    random_state = 1,
    mtry = 2,
    save_level = 2,
    save_prefix = "train_model1",
    overwrite = FALSE,
    output = "output/train_model1",
    verbose = TRUE
  )
  expect_true(is.list(tune_result))
  expect_true(length(tune_result) == 1)
})
