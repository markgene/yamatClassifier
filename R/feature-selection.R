#' Perform Boruta feature selection.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param with_tentative if set to \code{TRUE}, Tentative attributes will be
#'   also returned. See \code{\link[Boruta]{getSelectedAttributes}}.
#' @param ... parameters for \code{\link[Boruta]{Boruta}}.
#' @return a vector of selected features.
#' @export
select_features_boruta <- function(dat, response_name, with_tentative = FALSE, ...) {
  logger::log_debug("Running Boruta...")
  input_formula <- stats::as.formula(paste(response_name, "~ ."))
  boruta_result <- Boruta::Boruta(input_formula, data = dat, doTrace = 2, ...)
  selected_features <- Boruta::getSelectedAttributes(boruta_result, withTentative = with_tentative)
  logger::log_debug(glue::glue(
    "{length(selected_features)} features selected by Boruta algorithm."
  ))
  return(selected_features)
}


#' Perform Boruta feature selection with Boruta result.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param with_tentative if set to \code{TRUE}, Tentative attributes will be
#'   also returned. See \code{\link[Boruta]{getSelectedAttributes}}.
#' @param ... parameters for \code{\link[Boruta]{Boruta}}.
#' @return a list of two items: a vector of selected features and a \code{Boruta}
#'   object.
#' @export
select_features_boruta2 <- function(dat, response_name, with_tentative = FALSE, ...) {
  logger::log_debug("Running Boruta...")
  input_formula <- stats::as.formula(paste(response_name, "~ ."))
  boruta_result <- Boruta::Boruta(input_formula, data = dat, doTrace = 2, ...)
  selected_features <- Boruta::getSelectedAttributes(boruta_result, withTentative = with_tentative)
  logger::log_debug(glue::glue(
    "{length(selected_features)} features selected by Boruta algorithm."
  ))
  return(list(selected_features = selected_features, boruta_result = boruta_result))
}



#' Perform random forest feature selection with ranger package.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param top_n most important N features.
#' @param num_trees Number of trees. Default to 100 instead of 500 in \code{\link[ranger]{ranger}}.
#' @param importance see \code{\link[ranger]{ranger}} importance argument. Default to "permutation" here.
#' @param ... parameters for \code{\link[ranger]{ranger}}.
#' @return a vector of selected features.
#' @export
select_features_ranger <- function(dat,
                                   response_name,
                                   top_n = 10000,
                                   num_trees = 100,
                                   importance = "permutation",
                                   ...) {
  if (top_n > (ncol(dat) - 1)) {
    stop("top_n should be less than the number of features.")
  }
  logger::log_debug("Running ranger...")
  input_formula <- stats::as.formula(paste(response_name, "~ ."))
  rf_model_ranger <- ranger::ranger(
    input_formula,
    data = dat,
    num.trees = num_trees,
    importance = importance,
    ...
  )
  importance_scores <- rf_model_ranger$variable.importance
  # Sort features by importance
  sorted_importance <- sort(importance_scores, decreasing = TRUE)
  # Select the top 2 most important features
  top_features <- names(sorted_importance)[1:top_n]
  logger::log_debug(
    glue::glue(
      "{length(top_features)} features selected by random forest implemented by ranger."
    )
  )
  gc()
  return(top_features)
}
