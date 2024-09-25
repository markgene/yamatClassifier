#' Perform Boruta feature selection.
#'
#' @param dat a \code{data.frame} of input data.
#' @param response_name column name of the response.
#' @param ... parameters for \code{\link[Boruta]{Boruta}}.
#' @return a vector of selected features.
#' @export
select_features_boruta <- function(dat, response_name, ...) {
  logger::log_debug("Running Boruta...")
  input_formula <- stats::as.formula(paste(response_name, "~ ."))
  boruta_result <- Boruta::Boruta(input_formula, data = dat, doTrace = 2, ...)
  logger::log_debug(glue::glue("{selected_features} features selected by Boruta algorithm."))
  selected_features <- Boruta::getSelectedAttributes(boruta_result, withTentative = FALSE)
  return(selected_features)
}
