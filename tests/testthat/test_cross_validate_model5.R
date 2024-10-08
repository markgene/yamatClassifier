context("Test cross_validate_model5()")
library(yamatClassifier)
library(glmnet)
library(randomForest)

test_that("cross_validate_model5() end-to-end with mtry not set", {
  require(datasets)
  utils::data(iris)
  tune_result <- yamatClassifier::cross_validate_model5(
    dat = iris,
    response_name = "Species",
    outer_cv_folds = 3,
    inner_cv_folds = 3,
    random_state = 1,
    mtry = NULL,
    save_level = 2,
    save_prefix = "cross_validate_model5",
    overwrite = FALSE,
    output = "output/cross_validate_model5",
    verbose = TRUE
  )
  expect_true(is.list(tune_result))
  expect_true(length(tune_result) == 1)
})


# test_that("cross_validate_model5() end-to-end with mtry set to 2", {
#   require(datasets)
#   utils::data(iris)
#   tune_result <- yamatClassifier::cross_validate_model5(
#     dat = iris,
#     response_name = "Species",
#     outer_cv_folds = 3,
#     inner_cv_folds = 3,
#     random_state = 1,
#     mtry = 2,
#     save_level = 2,
#     save_prefix = "cross_validate_model5",
#     overwrite = FALSE,
#     output = "output/cross_validate_model5",
#     verbose = TRUE
#   )
#   expect_true(is.list(tune_result))
#   expect_true(length(tune_result) == 1)
# })
