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
    feature_selection = "Boruta",
    outer_cv_folds = 3,
    inner_cv_folds = 3,
    random_state = 1,
    mtry = 2,
    verbose = TRUE
  )
  expect_true(is.list(tune_result))
  expect_true(length(tune_result) == 1)
})
