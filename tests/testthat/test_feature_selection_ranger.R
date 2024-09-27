context("Test feature_selection_ranger()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("feature_selection_ranger()", {
  require(mlbench)
  utils::data(Sonar)
  set.seed(42)
  selected_features <- yamatClassifier::select_features_ranger(dat = Sonar,
                                                               response_name = "Class",
                                                               top_n = 10)
  expect_true(is.vector(selected_features))
  expect_true(length(selected_features) == 10)
})
