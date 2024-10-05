context("Test train_model3()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("train_model3() end-to-end", {
  require(datasets)
  utils::data(iris)
  result <- yamatClassifier::train_model3(
    dat = iris,
    response_name = "Species",
    outer_cv_folds = 3,
    inner_cv_folds = 3,
    random_state = 1,
    # mtry = 10,
    selected_features = c("Sepal.Length", "Sepal.Width"),
    importance = "permutation",
    save_level = 2,
    save_prefix = "train_model3",
    overwrite = FALSE,
    output = "output/train_model3",
    verbose = TRUE
  )
  expect_true(is.list(result))
  expect_true(length(result) == 1)
})
