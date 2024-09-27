context("Test train_model2()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("train_model2() end-to-end", {
  require(datasets)
  utils::data(iris)
  result <- yamatClassifier::train_model2(
    dat = iris,
    response_name = "Species",
    outer_cv_folds = 3,
    inner_cv_folds = 3,
    random_state = 1,
    # mtry = 10,
    top_n = 2,
    feature_selection_ranger_num_trees = 50,
    importance = "permutation",
    save_level = 2,
    save_prefix = "train_model2",
    overwrite = FALSE,
    output = "output/train_model2",
    verbose = TRUE
  )
  expect_true(is.list(result))
  expect_true(length(result) == 1)
})
