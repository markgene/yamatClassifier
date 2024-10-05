# Training model 3

#' Train model 3
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param outer_cv_folds outer cross-validation fold number.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param random_state random seed.
#' @param mtry A vector of mtry for parameter tuning.
#' @param selected_features features selected.
#' @param importance \code{importance} of \code{\link[ranger]{ranger}}.
#' @param save_level if save_level > 0, save outer train index. If save_level > 1,
#'   save calibrated probabilities and selected features in addition.
#' @param save_prefix output file prefix.
#' @param overwrite overwrite existing result files or not.
#' @param output output directory.
#' @param verbose A bool.
#' @return a list of cross-validation result of given \code{mtry} values.
#' @details Key steps:
#'   \enumerate{
#'     \item Tuning loop tunes single parameter \code{mtry}.
#'     \item Outer cross-validation split the data set into training and testing
#'       set of M folds.
#'     \item Inner cross-validation split the training set of the outer CV into
#'       N folds. Each fold does random forest classification. When all folds
#'       are done. Train calibration model of Ridge multinomial logistic
#'       regression (MR) regression. The lambda is trained with
#'       \code{\link[glmnet]{cv.glmnet}}. The random forest and calibration
#'       models are used for the testing set of the outer CV.
#'   }
#' @export
train_model3 <- function(dat,
                         response_name,
                         outer_cv_folds = 5,
                         inner_cv_folds = 5,
                         random_state = 56,
                         mtry = NULL,
                         selected_features = NULL,
                         importance = "permutation",
                         save_level = 3,
                         save_prefix = "train_model3_",
                         overwrite = FALSE,
                         output = NULL,
                         verbose = TRUE) {
  if (save_level > 0 && is.null(output)) {
    stop("output is required when save level > 0")
  }
  if (!is.null(output)) {
    dir.create(output, recursive = TRUE)
  }
  if (is.null(selected_features)) {
    stop("selected_features is required.")
  }
  if (!(response_name %in% colnames(dat))) {
    stop(glue::glue("fail to find variable {response_name}"))
  }
  responses <- factor(dat[, response_name, drop = TRUE])
  x <- dat[, -(which(colnames(dat) == response_name)), drop = FALSE]
  if (is.null(mtry)) {
    warning("mtry is not set and set to default")
    if (!is.null(responses) && !is.factor(responses)) {
      mtry_base <- max(floor(ncol(x) / 3), 1)
    } else {
      mtry_base <- floor(sqrt(ncol(x)))
    }
    mtry <- mtry_base
  }
  set.seed(random_state)
  outer_train_indexes_rda <- file.path(output,
                                       glue::glue("{save_prefix}outer_train_indexes.Rda"))
  if (file.exists(outer_train_indexes_rda)) {
    if (!overwrite) {
      logger::log_debug(
        glue::glue(
          "Loading outer CV folds: {outer_cv_folds} folds and random_state={random_state}"
        )
      )
      load(outer_train_indexes_rda)
    }

  } else {
    logger::log_debug(
      glue::glue(
        "Creating outer CV folds: {outer_cv_folds} folds and random_state={random_state}"
      )
    )
    outer_train_indexes <- caret::createFolds(responses,
                                              k = outer_cv_folds,
                                              list = TRUE,
                                              returnTrain = TRUE)
    if (save_level > 0) {
      save(outer_train_indexes, file = outer_train_indexes_rda)
    }
  }
  tune_result <- lapply(mtry, function(mtry_i) {
    rf_grid <- expand.grid(mtry = mtry_i)
    logger::log_debug(glue::glue("Use random forest parameters mtry={mtry_i}"))
    cv_result <- lapply(seq(length(outer_train_indexes)), function(i) {
      logger::log_debug(glue::glue("Outer fold #{i}"))
      calibrated_prob_response_i_rda <- file.path(
        output,
        glue::glue(
          "{save_prefix}calibrated_prob_response_mtry{mtry_i}_fold_{i}.Rda"
        )
      )
      if (file.exists(calibrated_prob_response_i_rda)) {
        if (!overwrite) {
          logger::log_debug(
            glue::glue(
              "Loading calibrated prob from {calibrated_prob_response_i_rda}"
            )
          )
          load(calibrated_prob_response_i_rda)
          return(calibrated_prob_response)
        }
      }
      outer_train_index <- outer_train_indexes[[i]]
      outer_fold_result <- train_model3_outer_fold(
        dat = dat,
        response_name = response_name,
        outer_train_index = outer_train_index,
        random_state = random_state + i,
        inner_cv_folds = inner_cv_folds,
        rf_grid = rf_grid,
        selected_features = selected_features,
        importance = importance,
        verbose = verbose
      )
      calibrated_prob_response <- outer_fold_result$calibrated_prob_response
      gc()
      if (save_level > 1) {
        logger::log_debug(
          glue::glue(
            "Saving calibrated prob and selected features into {calibrated_prob_response_i_rda}"
          )
        )
        selected_features <- outer_fold_result$selected_features
        save(calibrated_prob_response, selected_features, file = calibrated_prob_response_i_rda)
      }
      calibrated_prob_response
    })
    do.call(rbind, cv_result)
  })
  return(tune_result)
}


#' Train model 3 - Process the outer fold
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param outer_train_index outer train sample indexes.
#' @param random_state random seed.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param selected_features features selected.
#' @param importance \code{importance} of \code{\link[ranger]{ranger}}.
#' @param verbose A bool.
#' @return a list of two attributes, a \code{data.frame} of calibrated
#'   probabilities and response variable, and a vector of selected features.
train_model3_outer_fold <- function(dat,
                                    response_name,
                                    outer_train_index,
                                    random_state,
                                    inner_cv_folds = 5,
                                    rf_grid = NULL,
                                    selected_features = NULL,
                                    importance = "permutation",
                                    verbose = TRUE,
                                    ...) {
  if (is.null(rf_grid)) {
    stop("parameter tuning grid is required.")
  }
  outer_train <- dat[outer_train_index, ]
  outer_test <- dat[-outer_train_index, ]
  logger::log_debug("Downsampling by the minimum of sample number across the response levels")
  y <- outer_train[, response_name, drop = TRUE]
  min_n_sample <- min(table(y))
  set.seed(random_state + 1)
  outer_train_downsampled <- outer_train %>%
    dplyr::group_by(across(all_of(response_name))) %>%
    dplyr::sample_n(size = min_n_sample) %>%
    dplyr::ungroup()
  outer_train <- outer_train[, c(selected_features, response_name)]
  logger::log_debug("Inner cross validation")
  mods <- train_model3_inner_fold(
    outer_train,
    response_name = response_name,
    inner_cv_folds = inner_cv_folds,
    random_state = (random_state + 2),
    rf_grid = rf_grid,
    verbose = verbose
  )
  logger::log_debug("Run random forest model on outer fold testing data set")
  predicted_probs <- predict(mods$rf_model, newdata = outer_test, type = "prob") %>%
    as.matrix()
  logger::log_debug("Run calibration model on outer fold testing data set")
  calibrated_probs <- predict(
    mods$calibration_model,
    newx = predicted_probs,
    s = "lambda.min",
    type = "response"
  )
  calibrated_prob_response <- cbind(calibrated_probs, outer_test[, response_name, drop = FALSE])
  gc()
  return(
    list(
      selected_features = selected_features,
      calibrated_prob_response = calibrated_prob_response
    )
  )
}


#' Train model 3 - process the inner fold
#'
#' @param outer_train a \code{data.frame} of one fold of training set during
#'   nested cross-validation.
#' @param response_name column name of the response.
#' @param random_state random seed.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param verbose A bool.
#' @return a list of two models of random forest and calibration model respectively.
train_model3_inner_fold <- function(outer_train,
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
  train_control <- caret::trainControl(
    method = "cv",
    number = inner_cv_folds,
    classProbs = TRUE,
    summaryFunction = caret::multiClassSummary
  )
  # Train the Random Forest model
  logger::log_debug("Train random forest model")
  set.seed(random_state)
  input_formula <- stats::as.formula(paste(response_name, "~ ."))
  rf_model <- caret::train(
    input_formula,
    data = outer_train,
    method = "rf",
    trControl = train_control,
    tuneGrid = rf_grid,
    verbose = verbose
  )

  # Get predicted probabilities on the training data
  logger::log_debug("Train Ridge regression for calibration")
  predicted_probs <- predict(rf_model, newdata = outer_train, type = "prob") %>%
    as.matrix()
  # ridge_multiclass_model <- glmnet::glmnet(predicted_probs,
  #                                          outer_train[, response_name, drop = TRUE],
  #                                          family = "multinomial",
  #                                          alpha = 0)
  # calibrated_probs <- predict(ridge_multiclass_model,
  #                             newx = predicted_probs,
  #                             type = "response")
  cv_ridge_multiclass <- glmnet::cv.glmnet(predicted_probs,
                                           outer_train[, response_name, drop = TRUE],
                                           family = "multinomial",
                                           alpha = 0)
  # calibrated_probs <- predict(cv_ridge_multiclass,
  #                             newx = predicted_probs,
  #                             s = "lambda.min",
  #                             type = "response")
  gc()
  return(list(rf_model = rf_model, calibration_model = cv_ridge_multiclass))
}
