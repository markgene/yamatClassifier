context("Test feature_selection_boruta()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("feature_selection_boruta()", {
  require(mlbench)
  data(Sonar)
  set.seed(42)
  selected_features <- yamatClassifier::select_features_boruta(dat = Sonar, response_name = "Class")
  expect_true(is.vector(selected_features))
  expect_true(length(tune_result) == 31)
})
