context("Test feature_selection_boruta2()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("feature_selection_boruta2()", {
  require(mlbench)
  utils::data(Sonar)
  set.seed(42)
  output <- yamatClassifier::select_features_boruta2(dat = Sonar, response_name = "Class")
  selected_features <- output$selected_features
  boruta_result <- output$boruta_result
  expect_true(is.vector(selected_features))
  expect_true(length(selected_features) == 31)
  expect_true("Boruta" %in% class(boruta_result))
})
