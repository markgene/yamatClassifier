# Model 5

#' Cross-validate model 5
#'
#' Similar to model 1, but with tentative features from Boruta.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param feature_selection feature selection method.
#' @param outer_cv_folds outer cross-validation fold number.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param calibration_youden_index_threshold a float of Youden index threhold.
#'   Default to 0.9.
#' @param calibration_lambda_min_ratio see \code{lambda.min.ratio} of
#'   \code{\link[glmnet]{glmnet}}. Default to 1e-6.
#' @param random_state random seed.
#' @param mtry A vector of mtry for parameter tuning.
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
#'       N folds. Each fold does the feature selection with Boruta algorithm and
#'       random forest classification. When all folds are done. Train calibration
#'       model of Ridge multinomial logistic regression (MR) regression. The
#'       lambda is trained with \code{\link[glmnet]{cv.glmnet}}. The random
#'       forest and calibration models are used for the testing set of the outer
#'       CV.
#'   }
#' @export
cross_validate_model5 <- function(dat,
                                  response_name,
                                  outer_cv_folds = 3,
                                  inner_cv_folds = 3,
                                  calibration_youden_index_threshold = 0.9,
                                  calibration_lambda_min_ratio = 1e-6,
                                  random_state = 56,
                                  mtry = NULL,
                                  save_level = 3,
                                  save_prefix = "train_model5_",
                                  overwrite = FALSE,
                                  output = NULL,
                                  verbose = TRUE) {
  if (save_level > 0 && is.null(output)) {
    stop("output is required when save level > 0")
  }
  if (!is.null(output)) {
    dir.create(output, recursive = TRUE)
  }
  if (!(response_name %in% colnames(dat))) {
    stop(glue::glue("fail to find variable {response_name}"))
  }
  responses <- factor(dat[, response_name, drop = TRUE])
  x <- dat[, -(which(colnames(dat) == response_name)), drop = FALSE]
  if (is.null(mtry)) {
    warning("mtry is not set and set to default")
    mtry <- 0
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
    if (mtry_i == 0) {
      rf_grid <- NULL
      mtry_i <- "default"
    } else {
      rf_grid <- expand.grid(mtry = mtry_i)
    }
    logger::log_debug(glue::glue("Use random forest parameters mtry={mtry_i}"))
    cv_result <- lapply(seq(length(outer_train_indexes)), function(i) {
      logger::log_debug(glue::glue("Outer fold #{i}"))
      outer_fold_i_rda <- file.path(output,
                                    glue::glue("{save_prefix}outer_fold_mtry_{mtry_i}_fold_{i}.Rda"))
      if (file.exists(outer_fold_i_rda)) {
        if (!overwrite) {
          logger::log_debug(glue::glue("Loading calibrated prob from {outer_fold_i_rda}"))
          load(outer_fold_i_rda)
          return(calibrated_prob_response)
        }
      }
      outer_train_index <- outer_train_indexes[[i]]
      outer_fold_result <- train_model5_outer_fold(
        dat = dat,
        response_name = response_name,
        outer_train_index = outer_train_index,
        random_state = random_state + i,
        inner_cv_folds = inner_cv_folds,
        calibration_youden_index_threshold = calibration_youden_index_threshold,
        calibration_lambda_min_ratio = calibration_lambda_min_ratio,
        rf_grid = rf_grid,
        result_file = outer_fold_i_rda,
        verbose = verbose
      )
      outer_fold_result
    })
  })
  return(tune_result)
}


#' Train model 5 - Process the outer fold
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param outer_train_index outer train sample indexes.
#' @param random_state random seed.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param calibration_youden_index_threshold a float of Youden index threhold.
#'   Default to 0.9.
#' @param calibration_lambda_min_ratio see \code{lambda.min.ratio} of
#'   \code{\link[glmnet]{glmnet}}. Default to 1e-6.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param result_file save result in Rda file.
#' @param verbose A bool.
#' @return a list of two attributes, a \code{data.frame} of calibrated
#'   probabilities and response variable, and a vector of selected features.
train_model5_outer_fold <- function(dat,
                                    response_name,
                                    outer_train_index,
                                    random_state,
                                    inner_cv_folds = 3,
                                    calibration_youden_index_threshold = 0.9,
                                    calibration_lambda_min_ratio = 1e-6,
                                    rf_grid = NULL,
                                    result_file = "outer_fold.Rda",
                                    verbose = TRUE,
                                    ...) {
  outer_train <- dat[outer_train_index, ]
  outer_test <- dat[-outer_train_index, ]
  y <- outer_train[, response_name, drop = TRUE]
  logger::log_debug("Downsampling by the minimum of sample number across the response levels")
  min_n_sample <- min(table(y))
  set.seed(random_state + 1)
  outer_train_downsampled <- outer_train %>%
    dplyr::group_by(across(all_of(response_name))) %>%
    dplyr::sample_n(size = min_n_sample) %>%
    dplyr::ungroup()

  result_file_prefix <- sub(pattern = "(.*)\\..*$",
                            replacement = "\\1",
                            basename(result_file))
  boruta_result_rda <- paste0(result_file_prefix, "_boruta_result.Rda")
  if (file.exists(boruta_result_rda)) {
    logger::log_debug("Load pre-computed selected features with Boruta algorithm")
    load(boruta_result_rda)
  } else {
    logger::log_debug("Selecting features with Boruta algorithm")
    boruta_result <- select_features_boruta2(outer_train_downsampled,
                                             response_name = response_name,
                                             with_tentative = TRUE)
    selected_features <- boruta_result$selected_features
    save(selected_features, boruta_result, file = boruta_result_rda)
  }

  outer_train <- outer_train[, c(selected_features, response_name)]
  logger::log_debug("Inner cross validation")
  inner_fold_result <- train_model5_inner_fold(
    outer_train,
    response_name = response_name,
    inner_cv_folds = inner_cv_folds,
    random_state = (random_state + 2),
    calibration_youden_index_threshold = calibration_youden_index_threshold,
    calibration_lambda_min_ratio = calibration_lambda_min_ratio,
    rf_grid = rf_grid,
    verbose = verbose
  )
  logger::log_debug("Run random forest model on outer fold testing data set")
  outer_test_x <- as.matrix(outer_test[, selected_features])
  rf_probs <- predict(inner_fold_result$rf_model,
                      newdata = outer_test_x,
                      type = "prob") %>%
    as.matrix()
  rf_prob_response <- cbind(rf_probs, outer_test[, response_name, drop = FALSE])

  logger::log_debug(
    "Run calibration model maximizing average Youden index of inner fold  on outer fold testing data set"
  )
  calibrated_probs_max_avg_youden <- predict(
    inner_fold_result$calibration_result$ridge_model,
    newx = rf_probs,
    s = inner_fold_result$calibration_result$min_lambda_max_avg_youden,
    type = "response"
  )
  calibrated_prob_response_max_avg_youden <- cbind(calibrated_probs_max_avg_youden[, , 1], outer_test[, response_name, drop = FALSE])
  gc()

  # logger::log_debug(
  #   "Run calibration model minimizing misclassification rate of inner fold on outer fold testing data set"
  # )
  # calibrated_probs_min_misclassification <- predict(
  #   inner_fold_result$calibration_result$cv_ridge_model,
  #   newx = rf_probs,
  #   s = "lambda.min",
  #   type = "response"
  # )
  # calibrated_prob_response_min_misclassification <- cbind(calibrated_probs_min_misclassification[, , 1], outer_test[, response_name, drop = FALSE])
  # gc()
  logger::log_debug(glue::glue(
    "Saving calibrated prob and selected features into {result_file}"
  ))
  save(
    rf_prob_response,
    calibrated_prob_response_max_avg_youden,
    # calibrated_prob_response_min_misclassification,
    selected_features,
    boruta_result,
    calibrated_probs_max_avg_youden,
    outer_train,
    outer_test,
    inner_fold_result,
    file = result_file
  )
  return(
    list(
      selected_features = selected_features,
      rf_prob_response = rf_prob_response,
      calibrated_prob_response_max_avg_youden = calibrated_prob_response_max_avg_youden
    )
  )
}


#' Train model 5 - process the inner fold
#'
#' @param outer_train a \code{data.frame} of one fold of training set during
#'   nested cross-validation.
#' @param response_name column name of the response.
#' @param random_state random seed.
#' @param inner_cv_folds inner cross-validation fold number.
#' @param calibration_youden_index_threshold a float of Youden index threhold.
#'   Default to 0.9.
#' @param calibration_lambda_min_ratio see \code{lambda.min.ratio} of
#'   \code{\link[glmnet]{glmnet}}. Default to 1e-6.
#' @param rf_grid A data frame with possible tuning values. See \code{\link[caret]{train}}.
#' @param verbose A bool.
#' @return a list of two models of random forest and calibration model respectively.
train_model5_inner_fold <- function(outer_train,
                                    response_name,
                                    random_state,
                                    inner_cv_folds = 5,
                                    calibration_youden_index_threshold = 0.9,
                                    calibration_lambda_min_ratio = 1e-6,
                                    rf_grid = NULL,
                                    verbose = TRUE,
                                    ...) {
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
  calibration_result <- train_calibration_model_ridge(
    X = predicted_probs,
    y = outer_train[, response_name, drop = TRUE],
    youden_index_threshold = calibration_youden_index_threshold,
    lambda_min_ratio = calibration_lambda_min_ratio
  )
  gc()
  return(list(rf_model = rf_model, calibration_result = calibration_result))
}


#' Train calibration model with ridge regularization.
#'
#' @param X a matrix of predicted values.
#' @param y a factor vector of response.
#' @param youden_index_threshold the threshold. Default to 0.9.
#' @param lambda_min_ratio see \code{lambda.min.ratio} of \code{\link[glmnet]{glmnet}}.
#' @param cv_glmnet_nfolds see \code{nfolds} of \code{\link[glmnet]{cv.glmnet}}.
#' @returns calibration result.
#' @export
train_calibration_model_ridge <- function(X,
                                          y,
                                          youden_index_threshold = 0.9,
                                          lambda_min_ratio = 1e-6,
                                          cv_glmnet_nfolds = 3) {
  logger::log_debug("Calibration model to maximize average Youden index")
  # Fit ridge logistic regression model using glmnet
  ridge_model <- glmnet::glmnet(X,
                                y,
                                alpha = 0,
                                lambda.min.ratio = lambda_min_ratio,
                                family = "multinomial")

  # Get a sequence of lambda values that glmnet used to fit the model
  lambdas <- ridge_model$lambda

  # Predict probabilities for each lambda
  probs <- predict(ridge_model,
                   newx = X,
                   s = lambdas,
                   type = "response")

  # Function to calculate Youden Index for each class in one-vs-rest (OvR) manner
  youden_index_multinomial <- function(probs, true_labels, class, threshold = youden_index_threshold) {
    # Convert probabilities to predicted class (1 for positive class, 0 for rest)
    predicted_class <- ifelse(probs[, class] > threshold, 1, 0)

    # Convert true labels to binary (1 for class of interest, 0 for others)
    true_binary <- ifelse(true_labels == class, 1, 0)

    # Calculate confusion matrix components
    TP <- sum(predicted_class == 1 & true_binary == 1)
    TN <- sum(predicted_class == 0 & true_binary == 0)
    FP <- sum(predicted_class == 1 & true_binary == 0)
    FN <- sum(predicted_class == 0 & true_binary == 1)

    # Calculate sensitivity and specificity
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)

    # Calculate Youden Index for the class
    J <- sensitivity + specificity - 1
    return(J)
  }

  # Calculate the Youden Index for each lambda and for each class
  youden_values <- array(0, dim = c(length(lambdas), length(unique(y))))
  colnames(youden_values) <- levels(y)
  for (k in 1:length(lambdas)) {
    for (class in levels(y)) {
      j <- which(class == levels(y))
      youden_values[k, j] <- youden_index_multinomial(probs[, , k], y, class)
    }
  }

  # Average Youden Index across all classes for each lambda
  avg_youden_values <- rowMeans(youden_values)

  # Find the lambda that maximizes the average Youden Index
  # if there are multiple lambda with the same Youden index,
  # get the smallest one.
  min_lambda_max_avg_youden <- lambdas[which.max(avg_youden_values)]
  max_avg_youden_value <- max(avg_youden_values)

  # Alternatively, for a specific class (e.g., class 1)
  # class_1_optimal_lambda <- lambdas[which.max(youden_values[, 1])]

  # Fit the final model using the optimal lambda
  # According to glmnet, Avoid supplying a single value for lambda (for
  # predictions after CV use predict() instead).
  # final_model <- glmnet::glmnet(X,
  #                               y,
  #                               alpha = 0,
  #                               family = "multinomial",
  #                               lambda = optimal_lambda)

  # logger::log_debug("Calibration model to minimize misclassification error")
  # cv_ridge_model <- glmnet::cv.glmnet(
  #   X,
  #   y,
  #   family = "multinomial",
  #   alpha = 0,
  #   type.measure = "class",
  #   nfolds = cv_glmnet_nfolds
  # )
  return(
    list(
      X = X,
      y = y,
      youden_values = youden_values,
      ridge_model = ridge_model,
      # cv_ridge_model = cv_ridge_model,
      min_lambda_max_avg_youden = min_lambda_max_avg_youden,
      max_avg_youden_value = max_avg_youden_value
    )
  )
}
