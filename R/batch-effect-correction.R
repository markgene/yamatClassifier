# Train the model for batch effect removal.


#' Train the model for batch effect removal.
#'
#' @param x a matrix of log2 transformed signal intensity.
#' @param batch factor or vector indicating batches.
#' @param batch2 factor or vector indicating a second series of batches.
#' @param covariates matrix or vector of numeric covariates to be adjusted for.
#' @param design design matrix relating to treatment conditions to be
#'   preserved, usually the design matrix with all experimental factors other
#'   than the batch effects.
#' @param adjusted_rda the file save the adjusted value.
#' @param fit_rda the file save the model.
#' @param ... parameters pass to \code{\link[limma]{lmFit}}.
#' @returns a list of fit result.
#' @details Inspired by \code{\link[limma]{removeBatchEffect}}.
#' @export
train_batch_effect_model <- function(x,
                                     batch = NULL,
                                     batch2 = NULL,
                                     covariates = NULL,
                                     design = matrix(1, ncol(x), 1),
                                     adjusted_rda = "adjusted.Rda",
                                     fit_rda = "fit.Rda",
                                     ...) {
  if (is.null(batch) &&
      is.null(batch2) && is.null(covariates))
    return(as.matrix(x))
  X.batch <- get_batch_matrix(batch = batch,
                              batch2 = batch2,
                              covariates =  covariates)
  logger::log_debug("Fitting the model for batch effect correction...")
  fit <- limma::lmFit(x, cbind(design, X.batch), ...)
  bs <- fit$coefficients[, -(1:ncol(design)), drop = FALSE]
  bs[is.na(beta)] <- 0
  fit$coefficients_batch_effect <- bs
  logger::log_debug("Saving the model for batch effect correction...")
  save(fit, file = fit_rda)
  logger::log_debug("Adjusting for batch effect...")
  adjusted <- as.matrix(x) - bs %*% t(X.batch)
  save(adjusted, file = adjusted_rda)
}


#' Get the batch matrix for fitting batch effect correction.
#'
#' @param batch factor or vector indicating batches.
#' @param batch2 factor or vector indicating a second series of batches.
#' @param covariates matrix or vector of numeric covariates to be adjusted for.
#' @returns a matrix for batch.
#' @details Inspired by \code{\link[limma]{removeBatchEffect}}.
get_batch_matrix <- function(batch = NULL,
                             batch2 = NULL,
                             covariates = NULL) {
  logger::log_debug("Preparing the parameters...")
  if (!is.null(batch)) {
    batch <- as.factor(batch)
    contrasts(batch) <- contr.sum(levels(batch))
    batch <- model.matrix(~ batch)[, -1, drop = FALSE]
  }
  if (!is.null(batch2)) {
    batch2 <- as.factor(batch2)
    contrasts(batch2) <- contr.sum(levels(batch2))
    batch2 <- model.matrix(~ batch2)[, -1, drop = FALSE]
  }
  if (!is.null(covariates))
    covariates <- as.matrix(covariates)
  logger::log_debug("Getting the batch matrix...")
  X.batch <- cbind(batch, batch2, covariates)
  return(X.batch)
}
