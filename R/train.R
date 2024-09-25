# Training

#' Train and evaluate the model with random forest and ridge-penalized
#' multinomial logistic regression (MR)
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param feature_selection feature selection method.
#' @param outer_cv_folds outer cross-validation fold number.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param random_state random seed.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param verbose A bool.
#' @return To be added.
#' @export
train_rf_ridge <- function(dat,
                           response_name,
                           feature_selection = c("Boruta"),
                           outer_cv_folds = 5,
                           inner_cv_folds = 5,
                           random_state = 56,
                           rf_grid = NULL,
                           verbose = TRUE) {
  if (!(response_name %in% colnames(dat))) {
    stop(glue::glue("fail to find variable {response_name}"))
  }
  responses <- factor(dat[, response_name, drop = TRUE])
  x <- dat[, -(which(colnames(dat) == response_name)), drop = FALSE]
  if (is.null(rf_grid)) {
    warning("rf_grid is not set and set to default")
    if (!is.null(responses) && !is.factor(responses)) {
      mtry_base <- max(floor(ncol(x) / 3), 1)
    } else {
      mtry_base <- floor(sqrt(ncol(x)))
    }
    mtry <- mtry_base
    ntree <- 500
    rf_grid <- expand.grid(ntree = ntree, mtry = mtry)
  }
  set.seed(random_state)
  logger::log_debug(
    glue::glue(
      "Creating outer CV folds: {outer_cv_folds} folds and random_state={random_state}"
    )
  )
  outer_train_indexes <- caret::createFolds(responses,
                                            k = outer_cv_folds,
                                            list = TRUE,
                                            returnTrain = TRUE)
  rf_param_set_name <- sapply(seq(nrow(rf_grid)), function(j) {
    rf_params <- rf_grid[j, ]
    glue::glue("ntree{rf_params$ntree}_mtry{rf_params$mtry}")
  })
  tune_result <- lapply(seq(nrow(rf_grid)), function(j) {
    rf_params <- rf_grid[j, ]
    logger::log_debug(
      glue::glue(
        "Use random forest parameters ntree={rf_params$ntree}, mtry={rf_params$mtry}"
      )
    )
    cv_result <- lapply(seq(length(outer_train_indexes)), function(i) {
      outer_train_index <- outer_train_indexes[[i]]
      calibrated_prob_response <- train_rf_ridge_outer_fold(
        dat = dat,
        response_name = response_name,
        outer_train_index = outer_train_index,
        random_state = random_state + i,
        feature_selection = feature_selection,
        inner_cv_folds = inner_cv_folds,
        rf_grid = NULL,
        verbose = verbose
      )
    })
    do.call(cv_result, rbind)
  })
  names(tune_result) <- rf_param_set_name
  return(tune_result)
}


#' Process the outer fold
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param outer_train_index outer train sample indexes.
#' @param random_state random seed.
#' @param feature_selection feature selection method.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param verbose A bool.
#' @return a \code{data.frame} of calibrated probabilities and response variable.
train_rf_ridge_outer_fold <- function(dat,
                                      response_name,
                                      outer_train_index,
                                      random_state,
                                      feature_selection = c("Boruta"),
                                      inner_cv_folds = 5,
                                      rf_grid = NULL,
                                      verbose = TRUE,
                                      ...) {
  outer_train <- dat[outer_train_index, ]
  outer_test <- dat[-outer_train_index, ]
  if (feature_selection == "Boruta") {
    y <- outer_train[, response_name, drop = TRUE]
    logger::log_debug("Downsampling by the minimum of sample number across the response levels")
    min_n_sample <- min(table(y))
    set.seed(random_state + 1)
    outer_train_downsampled <- outer_train %>%
      dplyr::group_by(across(all_of(response_name))) %>%
      dplyr::sample_n(size = min_n_sample) %>%
      dplyr::ungroup()
    logger::log_debug("Selecting features with Boruta algorithm")
    selected_features <- select_features_boruta(outer_train_downsampled, response_name = response_name)
  }
  outer_train <- outer_train[, c(selected_features, response_name)]
  mods <- train_rf_ridge_inner_fold(
    outer_train,
    response_name = response_name,
    inner_cv_folds = inner_cv_folds,
    random_state = (random_state + 2),
    rf_grid = rf_grid,
    verbose = verbose
  )
  predicted_probs <- predict(mods$rf_model, newdata = outer_test, type = "prob")
  calibrated_probs <- predict(
    mods$calibration_model,
    newx = predicted_probs,
    s = "lambda.min",
    type = "response"
  )
  output <- cbind(calibrated_probs, outer_test[, response_name, drop = FALSE])
  return(output)
}


#' Process the inner fold
#'
#' @param outer_train a \code{data.frame} of one fold of training set during
#'   nested cross-validation.
#' @param response_name column name of the response.
#' @param random_state random seed.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param verbose A bool.
#' @return a list of two models of random forest and calibration model respectively.
train_rf_ridge_inner_fold <- function(outer_train,
                                      response_name,
                                      inner_cv_folds = 5,
                                      random_state,
                                      rf_grid = NULL,
                                      verbose = TRUE,
                                      ...) {
  if (is.null(rf_grid)) {
    stop("parameter tuning grid is required.")
  }
  responses <- factor(outer_train[, response_name, drop = TRUE])
  train_control <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = multiClassSummary
  )
  # Train the Random Forest model
  set.seed(random_state)
  input_formula <- as.formula(paste(response_name, "~ ."))
  rf_model <- train(
    input_formula,
    data = outer_train,
    method = "rf",
    trControl = train_control,
    tuneGrid = rf_grid,
    verbose = verbose
  )

  # Get predicted probabilities on the training data
  predicted_probs <- predict(rf_model, newdata = outer_train, type = "prob")
  ridge_multiclass_model <- glmnet::glmnet(predicted_probs,
                                           outer_train$Class,
                                           family = "multinomial",
                                           alpha = 0)
  cv_ridge_multiclass <- cv.glmnet(predicted_probs,
                                   outer_train$Class,
                                   family = "multinomial",
                                   alpha = 0)
  calibrated_probs <- predict(cv_ridge_multiclass,
                              newx = predicted_probs,
                              s = "lambda.min",
                              type = "response")
  return(rf_model = rf_model, calibration_model = ridge_multiclass_model)
}


#' Perform Boruta feature selection.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param ... parameters for \code{\link[Boruta]{Boruta}}.
#' @return a vector of selected features.
#' @export
select_features_boruta <- function(dat, response_name, ...) {
  logger::log_debug("Running Boruta...")
  input_formula <- as.formula(paste(response_name, "~ ."))
  boruta_result <- Boruta::Boruta(input_formula, data = outer_train, doTrace = 2, ...)
  logger::log_debug(glue::glue("{selected_features} features selected by Boruta algorithm."))
  selected_features <- Boruta::getSelectedAttributes(boruta_result, withTentative = FALSE)
  return(selected_features)
}
