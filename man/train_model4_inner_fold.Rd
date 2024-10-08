% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train_model4.R
\name{train_model4_inner_fold}
\alias{train_model4_inner_fold}
\title{Train model 4 - process the inner fold}
\usage{
train_model4_inner_fold(
  outer_train,
  response_name,
  inner_cv_folds = 5,
  random_state,
  rf_grid = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{outer_train}{a \code{data.frame} of one fold of training set during
nested cross-validation.}

\item{response_name}{column name of the response.}

\item{inner_cv_folds}{inner cross-validation fold number.}

\item{random_state}{random seed.}

\item{rf_grid}{A data frame with possible tuning values. See \code{\link[caret]{train}}.}

\item{verbose}{A bool.}
}
\value{
a list of two models of random forest and calibration model respectively.
}
\description{
Train model 4 - process the inner fold
}
